
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Internal.LIO
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
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
        invokeLIO,
        runLIO,
        lioParams,
        nextLIOParams,
        rootLIOParams,
        parentLIOParams,
        bestSuitedLIOParams,
        bestSuitedLIOParamsButLess,
        latticeTimeIndex,
        latticeMemberIndex,
        latticeStartTime) where

import Data.IORef
import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Exception (throw, catch, finally)

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Internal.Types

-- | The 'LIO' computation that can be run as nested one on the lattice node.
newtype LIO a = LIO { unLIO :: LIOParams -> IO a
                      -- ^ Unwrap the computation.
                    }

-- | The parameters of the 'LIO' computation.
data LIOParams =
  LIOParams { lioTimeIndex :: !Int,
              -- ^ The time index.
              lioMemberIndex :: !Int
              -- ^ The member index.
            } deriving (Eq, Ord, Show)

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

-- | Run the 'LIO' computation using the integration times points as lattice nodes.
runLIO :: LIO a -> IO a
runLIO m = unLIO m rootLIOParams

-- | Return the parameters of the computation.
lioParams :: LIO LIOParams
lioParams = LIO return

-- | Return the root node parameters.
rootLIOParams :: LIOParams
rootLIOParams = LIOParams { lioTimeIndex = 0,
                            lioMemberIndex = 0 }

-- | Return the parent parameters.
parentLIOParams :: LIOParams -> Maybe LIOParams
parentLIOParams ps
  | i == 0    = Nothing
  | otherwise = Just $ ps { lioTimeIndex = i - 1, lioMemberIndex = max 0 (k - 1) }
  where i = lioTimeIndex ps
        k = lioMemberIndex ps

-- | Return the fair parameters corresponding to the best suited lattice node.
bestSuitedLIOParams :: Event LIO LIOParams
bestSuitedLIOParams =
  Event $ \p ->
  LIO $ \ps ->
  let sc = runSpecs (pointRun p)
      i' = floor $ (pointTime p - spcStartTime sc) / spcDT sc
  in return $ ps { lioTimeIndex = i' }

-- | Return the fair parameters corresponding to the best suited lattice node but less parameters.
bestSuitedLIOParamsButLess :: Event LIO LIOParams
bestSuitedLIOParamsButLess =
  Event $ \p ->
  LIO $ \ps ->
  let sc = runSpecs (pointRun p)
      i  = lioTimeIndex ps
      i' = floor $ (pointTime p - spcStartTime sc) / spcDT sc
  in return $ ps { lioTimeIndex = max i i' }

-- | Return parameters for the next nodes.
nextLIOParams :: Event LIO (LIOParams, LIOParams)
nextLIOParams =
  Event $ \p ->
  LIO $ \ps ->
  let sc  = runSpecs (pointRun p)
      ps1 = ps { lioTimeIndex = 1 + i', lioMemberIndex = k' }
      ps2 = ps { lioTimeIndex = 1 + i', lioMemberIndex = 1 + k' }
      i'  = floor $ (pointTime p - spcStartTime sc) / spcDT sc
      k   = lioMemberIndex ps
      k'  = min i' k
  in return (ps1, ps2)

-- | Return the lattice time index starting from 0. It corresponds to the integration time point.
latticeTimeIndex :: LIO Int
latticeTimeIndex = LIO $ return . lioTimeIndex

-- | Return the lattice member index starting from 0. It is always less than or equaled to 'latticeTimeIndex'.
latticeMemberIndex :: LIO Int
latticeMemberIndex = LIO $ return . lioMemberIndex

-- | Return the start time for the current lattice node.
latticeStartTime :: Parameter LIO Double
latticeStartTime =
  Parameter $ \r ->
  LIO $ \ps ->
  let sc = runSpecs r
      i  = lioTimeIndex ps
      t  = spcStartTime sc + (fromInteger $ toInteger i) * (spcDT sc)
  in return t
