
{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Internal.Estimate
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the 'Estimate' monad transformer which is destined for estimating
-- computations within lattice nodes. Such computations are separated from the 'Event'
-- computations. An idea is that the forward-traversing 'Event' computations provide with
-- something that can be observed, while the backward-traversing 'Estimate' computations
-- estimate the received information.
--
module Simulation.Aivika.Lattice.Internal.Estimate
       (-- * Estimate Monad
        Estimate(..),
        EstimateLift(..),
        invokeEstimate,
        runEstimateInStartTime,
        estimateTime,
        -- * Error Handling
        catchEstimate,
        finallyEstimate,
        throwEstimate,
        -- * Debugging
        traceEstimate) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Applicative

import Debug.Trace (trace)

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Lattice.Internal.LIO

-- | A value in the 'Estimate' monad transformer represents something
-- that can be estimated within lattice nodes.
newtype Estimate m a = Estimate (Point m -> m a)

-- | Invoke the 'Estimate' computation.
invokeEstimate :: Point m -> Estimate m a -> m a
{-# INLINE invokeEstimate #-}
invokeEstimate p (Estimate m) = m p

instance Monad m => Monad (Estimate m) where

  {-# INLINE return #-}
  return a = Estimate $ \p -> return a

  {-# INLINE (>>=) #-}
  (Estimate m) >>= k =
    Estimate $ \p -> 
    do a <- m p
       let Estimate m' = k a
       m' p

instance Functor m => Functor (Estimate m) where
  
  {-# INLINE fmap #-}
  fmap f (Estimate x) = Estimate $ \p -> fmap f $ x p

instance Applicative m => Applicative (Estimate m) where
  
  {-# INLINE pure #-}
  pure = Estimate . const . pure
  
  {-# INLINE (<*>) #-}
  (Estimate x) <*> (Estimate y) = Estimate $ \p -> x p <*> y p

instance MonadTrans Estimate where

  {-# INLINE lift #-}
  lift = Estimate . const

instance MonadIO m => MonadIO (Estimate m) where
  
  {-# INLINE liftIO #-}
  liftIO = Estimate . const . liftIO

instance MonadFix m => MonadFix (Estimate m) where

  {-# INLINE mfix #-}
  mfix f = 
    Estimate $ \p ->
    do { rec { a <- invokeEstimate p (f a) }; return a }

instance Monad m => MonadCompTrans Estimate m where

  {-# INLINE liftComp #-}
  liftComp = Estimate . const

-- | A type class to lift the 'Estimate' computations into other computations.
class EstimateLift t m where
  
  -- | Lift the specified 'Estimate' computation into another computation.
  liftEstimate :: Estimate m a -> t m a

instance Monad m => EstimateLift Estimate m where
  
  {-# INLINE liftEstimate #-}
  liftEstimate = id

instance Monad m => ParameterLift Estimate m where

  {-# INLINE liftParameter #-}
  liftParameter (Parameter x) = Estimate $ x . pointRun

-- | Exception handling within 'Estimate' computations.
catchEstimate :: (MonadException m, Exception e) => Estimate m a -> (e -> Estimate m a) -> Estimate m a
{-# INLINABLE catchEstimate #-}
catchEstimate (Estimate m) h =
  Estimate $ \p -> 
  catchComp (m p) $ \e ->
  let Estimate m' = h e in m' p
                           
-- | A computation with finalization part like the 'finally' function.
finallyEstimate :: MonadException m => Estimate m a -> Estimate m b -> Estimate m a
{-# INLINABLE finallyEstimate #-}
finallyEstimate (Estimate m) (Estimate m') =
  Estimate $ \p ->
  finallyComp (m p) (m' p)

-- | Like the standard 'throw' function.
throwEstimate :: (MonadException m, Exception e) => e -> Estimate m a
{-# INLINABLE throwEstimate #-}
throwEstimate e =
  Estimate $ \p ->
  throwComp e

-- | Run the 'Estimate' computation in the start time and return the estimate.
runEstimateInStartTime :: MonadDES m => Estimate m a -> Simulation m a
{-# INLINE runEstimateInStartTime #-}
runEstimateInStartTime (Estimate m) = runEventInStartTime (Event m)

-- | Like 'time' estimates the current modeling time.
-- It is more effcient than 'latticeTime'.
estimateTime :: MonadDES m => Estimate m Double
{-# INLINE estimateTime #-}
estimateTime = Estimate $ return . pointTime

-- | Show the debug message with the current simulation time and lattice node indices.
traceEstimate :: String -> Estimate LIO a -> Estimate LIO a
{-# INLINABLE traceEstimate #-}
traceEstimate message m =
  Estimate $ \p ->
  LIO $ \ps ->
  trace ("t = " ++ show (pointTime p) ++
         ", lattice time index = " ++ show (lioTimeIndex ps) ++
         ", lattice member index = " ++ show (lioMemberIndex ps) ++
         ": " ++ message) $
  invokeLIO ps $
  invokeEstimate p m
