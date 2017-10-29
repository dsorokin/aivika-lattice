
-- |
-- Module     : Simulation.Aivika.Lattice.Estimate
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Estimate' monad transformer which is destined for estimating
-- computations within lattice nodes. Such computations are separated from the 'Event'
-- computations. An idea is that the forward-traversing 'Event' computations provide with
-- something that can be observed, while the 'Estimate' computations estimate the received
-- information and they can be backward-traversing.
--
module Simulation.Aivika.Lattice.Estimate
       (-- * Estimate Monad
        Estimate,
        EstimateLift(..),
        runEstimateInStartTime,
        estimateTime,
        -- * Computations within Lattice
        foldEstimate,
        memoEstimate,
        estimateUpSide,
        estimateDownSide,
        estimateFuture,
        shiftEstimate,
        estimateAt,
        -- * Error Handling
        catchEstimate,
        finallyEstimate,
        throwEstimate,
        -- * Debugging
        traceEstimate) where

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Ref
import Simulation.Aivika.Trans.Observable
import Simulation.Aivika.Lattice.Internal.Estimate
import Simulation.Aivika.Lattice.Internal.LIO
import qualified Simulation.Aivika.Lattice.Internal.Ref as R

-- | Estimate the computation in the lattice nodes.
memoEstimate :: (Estimate LIO a -> Estimate LIO a)
                -- ^ estimate in the intermediate time point of the lattice
                -> Estimate LIO a
                -- ^ estimate in the final time point of the lattice or beyond it
                -> Simulation LIO (Estimate LIO a)
memoEstimate f m =
  do r  <- R.newRef Nothing
     t2 <- liftParameter stoptime
     let loop =
           Estimate $ \p ->
           do b <- R.readRef0 r
              case b of
                Just a  -> return a
                Nothing ->
                  if pointTime p >= t2
                  then do a <- invokeEstimate p m
                          R.writeRef0 r (Just a)
                          return a
                  else do a <- invokeEstimate p (f loop)
                          R.writeRef0 r (Just a)
                          return a
     return loop

-- | Estimate the computation in the up side node of the lattice,
-- where 'latticeTimeIndex' is increased by 1 but 'latticeMemberIndex' remains the same.
--
-- It is merely equivalent to the following definition:
--
-- @estimateUpSide = shiftEstimate 1 0@
--
estimateUpSide :: Estimate LIO a -> Estimate LIO a
estimateUpSide m =
  Estimate $ \p ->
  LIO $ \ps ->
  do let ps' = upSideLIOParams ps
         r   = pointRun p
     p' <- invokeLIO ps' $
           invokeParameter r
           latticePoint
     invokeLIO ps' $
       invokeEstimate p' m

-- | Estimate the computation in the down side node of the lattice,
-- where the both 'latticeTimeIndex' and 'latticeMemberIndex' are increased by 1.
--
-- It is merely equivalent to the following definition:
--
-- @estimateDownSide = shiftEstimate 1 1@
--
estimateDownSide :: Estimate LIO a -> Estimate LIO a
estimateDownSide m =
  Estimate $ \p ->
  LIO $ \ps ->
  do let ps' = downSideLIOParams ps
         r   = pointRun p
     p' <- invokeLIO ps' $
           invokeParameter r
           latticePoint
     invokeLIO ps' $
       invokeEstimate p' m

-- | Estimate the computation in the shifted lattice node, where the first parameter
-- specifies the 'latticeTimeIndex' shift of any sign, but the second parameter
-- specifies the 'latticeMemberIndex' shift af any sign too.
--
-- It allows looking into the future or past computations. The lattice is constructed in such a way
-- that we can define the past 'Estimate' computation in terms of the future @Estimate@
-- computation. That is the point.
--
-- Regarding the 'Event' computation, it is quite different. The future @Event@ computation
-- depends strongly on the past @Event@ computations. But we can update 'Ref' references within
-- the corresponding discrete event simulation and then read them within the @Estimate@
-- computation, because @Ref@ is 'Observable'.
shiftEstimate :: Int
                 -- ^ a shift of the lattice time index
                 -> Int
                 -- ^ a shift of the lattice member index
                 -> Estimate LIO a
                 -- ^ the source computation
                 -> Estimate LIO a
shiftEstimate di dk m =
  Estimate $ \p ->
  LIO $ \ps ->
  do let ps' = shiftLIOParams di dk ps
         r   = pointRun p
     p' <- invokeLIO ps' $
           invokeParameter r
           latticePoint
     invokeLIO ps' $
       invokeEstimate p' m

-- | Like 'shiftEstimate' but only the first argument must be possitive.
estimateFuture :: Int
                  -- ^ a positive shift of the lattice time index
                  -> Int
                  -- ^ a shift of the lattice member index
                  -> Estimate LIO a
                  -- ^ the source computation
                  -> Estimate LIO a
estimateFuture di dk m
  | di <= 0   = error "Expected to see a positive time index shift: estimateFuture"
  | otherwise = shiftEstimate di dk m

-- | Estimate the computation at the specified 'latticeTimeIndex' and 'latticeMemberIndex'.
estimateAt :: Int
              -- ^ the lattice time index
              -> Int
              -- ^ the lattice size index
              -> Estimate LIO a
              -- ^ the computation
              -> Estimate LIO a
estimateAt i k m =
  Estimate $ \p ->
  LIO $ \ps ->
  do let ps' = lioParamsAt i k ps
         r   = pointRun p
     p' <- invokeLIO ps' $
           invokeParameter r
           latticePoint
     invokeLIO ps' $
       invokeEstimate p' m

-- | Fold the estimation of the specified computation.
foldEstimate :: (a -> a -> Estimate LIO a)
                -- ^ reduce in the intermediate nodes of the lattice
                -> Estimate LIO a
                -- ^ estimate the computation in the final time point and beyond it
                -> Simulation LIO (Estimate LIO a)
foldEstimate f = memoEstimate g
  where g m =
          do a1 <- estimateUpSide m
             a2 <- estimateDownSide m
             f a1 a2
