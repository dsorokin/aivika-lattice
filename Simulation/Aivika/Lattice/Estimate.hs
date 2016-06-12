
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
        -- * Error Handling
        catchEstimate,
        finallyEstimate,
        throwEstimate,
        -- * Debugging
        traceEstimate) where

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Lattice.Internal.Estimate
import Simulation.Aivika.Lattice.Internal.LIO
import qualified Simulation.Aivika.Lattice.Internal.Ref as R

-- | Estimate the event computation in the lattice nodes by the specified time.
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
