
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Internal.LIO
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines the 'LIO' computation.
--
module Simulation.Aivika.Lattice.Internal.LIO
       (LIOParams(..),
        LIO(..),
        LIOLattice(..),
        lattice,
        newRandomLattice,
        newRandomLatticeWithProb,
        invokeLIO,
        runLIO,
        lioParams,
        rootLIOParams,
        parentLIOParams,
        upSideLIOParams,
        downSideLIOParams,
        shiftLIOParams,
        lioParamsAt,
        latticeTimeIndex,
        latticeMemberIndex,
        latticeParentMemberIndex,
        latticeTime,
        latticeTimes,
        latticeTimeStep,
        latticePoint,
        latticeSize,
        findLatticeTimeIndex) where

import Data.IORef
import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Exception (throw, catch, finally)

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Parameter

import Simulation.Aivika.Lattice.Internal.Lattice

-- | The 'LIO' computation that can be run as nested one on the lattice node.
newtype LIO a = LIO { unLIO :: LIOParams -> IO a
                      -- ^ Unwrap the computation.
                    }

-- | The parameters of the 'LIO' computation.
data LIOParams =
  LIOParams { lioLattice :: LIOLattice,
              -- ^ The lattice.
              lioTimeIndex :: !Int,
              -- ^ The time index.
              lioMemberIndex :: !Int
              -- ^ The member index.
            }

instance Monad LIO where

  {-# INLINE return #-}
  return = LIO . const . return

  {-# INLINE (>>=) #-}
  (LIO m) >>= k = LIO $ \ps ->
    m ps >>= \a ->
    let m' = unLIO (k a) in m' ps

instance Applicative LIO where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Functor LIO where

  {-# INLINE fmap #-}
  fmap f (LIO m) = LIO $ fmap f . m 

instance MonadIO LIO where

  {-# INLINE liftIO #-}
  liftIO = LIO . const . liftIO

instance MonadFix LIO where

  mfix f = 
    LIO $ \ps ->
    do { rec { a <- invokeLIO ps (f a) }; return a }

instance MonadException LIO where

  catchComp (LIO m) h = LIO $ \ps ->
    catch (m ps) (\e -> unLIO (h e) ps)

  finallyComp (LIO m1) (LIO m2) = LIO $ \ps ->
    finally (m1 ps) (m2 ps)
  
  throwComp e = LIO $ \ps ->
    throw e

-- | Invoke the computation.
invokeLIO :: LIOParams -> LIO a -> IO a
{-# INLINE invokeLIO #-}
invokeLIO ps (LIO m) = m ps

-- | Run the 'LIO' computation using the specified lattice.
runLIO :: LIOLattice -> LIO a -> IO a
runLIO lattice m = unLIO m $ rootLIOParams lattice

-- | Return the parameters of the computation.
lioParams :: LIO LIOParams
lioParams = LIO return

-- | Return the root node parameters.
rootLIOParams :: LIOLattice -> LIOParams
rootLIOParams lattice =
  LIOParams { lioLattice = lattice,
              lioTimeIndex = 0,
              lioMemberIndex = 0 }

-- | Return the parent parameters.
parentLIOParams :: LIOParams -> Maybe LIOParams
parentLIOParams ps
  | i == 0    = Nothing
  | otherwise = Just $ ps { lioTimeIndex = i - 1, lioMemberIndex = k' }
  where i  = lioTimeIndex ps
        k  = lioMemberIndex ps
        k' = lioParentMemberIndex (lioLattice ps) i k

-- | Return the next up side parameters.
upSideLIOParams :: LIOParams -> LIOParams
upSideLIOParams ps = ps { lioTimeIndex = 1 + i }
  where i = lioTimeIndex ps

-- | Return the next down side parameters.
downSideLIOParams :: LIOParams -> LIOParams
downSideLIOParams ps = ps { lioTimeIndex = 1 + i, lioMemberIndex = 1 + k }
  where i = lioTimeIndex ps
        k = lioMemberIndex ps

-- | Return the derived parameters with the specified shift in 'latticeTimeIndex' and
-- 'latticeMemberIndex' respectively, where the first parameter can be positive only.
shiftLIOParams :: Int
                  -- ^ a positive shift the lattice time index
                  -> Int
                  -- ^ a shift of the lattice member index
                  -> LIOParams
                  -- ^ the source parameters
                  -> LIOParams
shiftLIOParams di dk ps
  | i' < 0    = error "The time index cannot be negative: shiftLIOParams"
  | k' < 0    = error "The member index cannot be negative: shiftLIOParams"
  | k' > i'   = error "The member index cannot be greater than the time index: shiftLIOParams"
  | otherwise = ps { lioTimeIndex = i', lioMemberIndex = k' }
  where i  = lioTimeIndex ps
        i' = i + di
        k  = lioMemberIndex ps
        k' = k + dk

-- | Return the parameters at the specified 'latticeTimeIndex' and 'latticeMemberIndex'.
lioParamsAt :: Int
               -- ^ the lattice time index
               -> Int
               -- ^ the lattice member index
               -> LIOParams
               -- ^ the source parameters
               -> LIOParams
lioParamsAt i k ps
  | i < 0     = error "The time index cannot be negative: lioParamsAt"
  | k < 0     = error "The member index cannot be negative: lioParamsAt"
  | k > i     = error "The member index cannot be greater than the time index: lioParamsAt"
  | otherwise = ps { lioTimeIndex = i, lioMemberIndex = k }

-- | Return the lattice time index starting from 0. The index should be less than or equaled to 'latticeSize'. 
latticeTimeIndex :: LIO Int
latticeTimeIndex = LIO $ return . lioTimeIndex

-- | Return the lattice member index starting from 0. It is always less than or equaled to 'latticeTimeIndex'.
latticeMemberIndex :: LIO Int
latticeMemberIndex = LIO $ return . lioMemberIndex

-- | Return the parent member index starting from 0 for non-root lattice nodes.
latticeParentMemberIndex :: LIO (Maybe Int)
latticeParentMemberIndex = LIO $ return . fmap lioMemberIndex . parentLIOParams

-- | Return the time for the current lattice node.
latticeTime :: Parameter LIO Double
latticeTime =
  Parameter $ \r ->
  LIO $ \ps ->
  let i = lioTimeIndex ps
  in invokeLIO ps $
     invokeParameter r $
     getLatticeTimeByIndex i

-- | Return the time values in the lattice nodes.
latticeTimes :: Parameter LIO [Double]
latticeTimes =
  Parameter $ \r ->
  LIO $ \ps ->
  let m  = lioSize $ lioLattice ps
  in forM [0 .. m] $ \i ->
     invokeLIO ps $
     invokeParameter r $
     getLatticeTimeByIndex i

-- | Return the point in the corresponding lattice node.
latticePoint :: Parameter LIO (Point LIO)
latticePoint =
  Parameter $ \r ->
  do t <- invokeParameter r latticeTime
     return $ pointAt r t

-- | Return the lattice time step.
latticeTimeStep :: Parameter LIO Double
latticeTimeStep =
  Parameter $ \r ->
  LIO $ \ps ->
  do let sc = runSpecs r
         t0 = spcStartTime sc
         t2 = spcStopTime sc
         m  = lioSize $ lioLattice ps
         dt = (t2 - t0) / (fromIntegral m)
     return dt

-- | Return the lattice size.
latticeSize :: LIO Int
latticeSize = LIO $ return . lioSize . lioLattice

-- | Find the lattice time index by the specified modeling time.
findLatticeTimeIndex :: Double -> Parameter LIO Int
findLatticeTimeIndex t =
  Parameter $ \r ->
  LIO $ \ps ->
  do let sc = runSpecs r
         t0 = spcStartTime sc
         t2 = spcStopTime sc
         m  = lioSize $ lioLattice ps
         i | t == t0   = 0
           | t == t2   = m
           | otherwise = floor (fromIntegral m * ((t - t0) / (t2 - t0)))
     return i

-- | Get the modeling time in the lattice node by the specified time index. 
getLatticeTimeByIndex :: Int -> Parameter LIO Double
getLatticeTimeByIndex i =
  Parameter $ \r ->
  LIO $ \ps ->
  let sc = runSpecs r
      t0 = spcStartTime sc
      t2 = spcStopTime sc
      m  = lioSize $ lioLattice ps
      dt = (t2 - t0) / (fromIntegral m)
      t | i == 0    = t0
        | i == m    = t2
        | otherwise = t0 + (fromInteger $ toInteger i) * dt
  in return t
