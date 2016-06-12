
-- |
-- Module     : Simulation.Aivika.Lattice.Estimate
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Estimate' monad transformer which is destined for providing
-- with estimating computations within lattice nodes. Such computations are separated
-- from the 'Event' computations. An idea is that the 'Event' computations provides with
-- something that can be observed, while the 'Estimate' computations estimate
-- the received information.
--
module Simulation.Aivika.Lattice.Estimate
       (-- * Estimate Monad
        Estimate,
        EstimateLift(..),
        runEstimateInStartTime,
        estimateTime,
        -- * Computations within Lattice
        memoEstimate,
        estimateUpSide,
        estimateDownSide,
        estimateFuture,
        latticeTimeStep,
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
-- @estimateUpSide = estimateFuture 1 0@
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
-- @estimateDownSide = estimateFuture 1 1@
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
-- specifies the positive 'latticeTimeIndex' shift, but the second parameter
-- specifies the 'latticeMemberIndex' shift.
--
-- It allows looking into the future computations. The lattice is constructed in such a way
-- that we can define the past 'Estimate' computations in terms of the future 'Estimate'
-- computations. That is the point.
--
-- Regarding the 'Event' computation, a quite opposite rule is true. The future 'Event' computation
-- depends on the past 'Event' computations. But we can update 'Ref' references within
-- the corresponding discrete event simulation and then read them within the 'Estimate'
-- computation, because 'Ref' is 'Observable'.
estimateFuture :: Int
                  -- ^ a positive shift of the lattice time index
                  -> Int
                  -- ^ a shift of the lattice member index
                  -> Estimate LIO a
                  -- ^ the source computation
                  -> Estimate LIO a
estimateFuture di dk m =
  Estimate $ \p ->
  LIO $ \ps ->
  do let ps' = shiftLIOParams di dk ps
         r   = pointRun p
     p' <- invokeLIO ps' $
           invokeParameter r
           latticePoint
     invokeLIO ps' $
       invokeEstimate p' m
