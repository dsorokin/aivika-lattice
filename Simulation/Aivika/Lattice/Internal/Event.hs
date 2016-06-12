
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Internal.Event
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The module defines an event queue, where 'LIO' is an instance of 'EventQueueing'.
-- Also it defines basic functions for running nested computations within lattice nodes.
--
module Simulation.Aivika.Lattice.Internal.Event
       (estimateRef) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import qualified Simulation.Aivika.PriorityQueue.Pure as PQ

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Lattice.Internal.LIO
import Simulation.Aivika.Lattice.Internal.Estimate
import qualified Simulation.Aivika.Lattice.Internal.Ref as R

-- | An implementation of the 'EventQueueing' type class.
instance EventQueueing LIO where

  -- | The event queue type.
  data EventQueue LIO =
    EventQueueLIO { queuePQ :: R.Ref (PQ.PriorityQueue (Point LIO -> LIO ())),
                    -- ^ the underlying priority queue
                    queueBusy :: IORef Bool,
                    -- ^ whether the queue is currently processing events
                    queueTime :: R.Ref Double
                    -- ^ the actual time of the event queue
                  }

  newEventQueue specs =
    do f  <- liftIO $ newIORef False
       t  <- R.newRef0 (spcStartTime specs)
       pq <- R.newRef0 PQ.emptyQueue
       return EventQueueLIO { queuePQ   = pq,
                              queueBusy = f,
                              queueTime = t }

  enqueueEvent t (Event m) =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in invokeEvent p $
       R.modifyRef pq $ \x -> PQ.enqueue x t m

  runEventWith processing (Event e) =
    Dynamics $ \p ->
    LIO $ \ps ->
    do invokeLIO ps $
         invokeDynamics p $
         processEvents processing
       invokeLIO ps $
         e p

  eventQueueCount =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in invokeEvent p $
       fmap PQ.queueCount $ R.readRef pq

-- | Process the pending events.
processPendingEventsCore :: Bool -> Dynamics LIO ()
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    LIO $ \ps ->
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- readIORef f
       if f'
         then error $
              "Detected an event loop, which may indicate to " ++
              "a logical error in the model: processPendingEventsCore"
         else do writeIORef f True
                 invokeLIO ps $
                   invokeDynamics p $
                   processPendingEventsUnsafe includingCurrentEvents
                 writeIORef f False

-- | Process the pending events in unsafe manner.
processPendingEventsUnsafe :: Bool -> Dynamics LIO ()
processPendingEventsUnsafe includingCurrentEvents = Dynamics r where
  r p =
    LIO $ \ps ->
    let q = runEventQueue $ pointRun p
    in call q p ps
  call q p ps =
    do let pq = queuePQ q
           r  = pointRun p
       f <- invokeLIO ps $
            fmap PQ.queueNull $ R.readRef0 pq
       unless f $
         do (t2, c2) <- invokeLIO ps $
                        fmap PQ.queueFront $ R.readRef0 pq
            let t = queueTime q
            t' <- invokeLIO ps $
                  R.readRef0 t
            when (t2 < t') $ 
              error "The time value is too small: processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do let sc = pointSpecs p
                     t0 = spcStartTime sc
                     dt = spcDT sc
                     n2 = fromIntegral $ floor ((t2 - t0) / dt)
                     p2 = p { pointTime = t2,
                              pointIteration = n2,
                              pointPhase = -1 }
                 invokeLIO ps $
                   R.writeRef0 t t2
                 invokeLIO ps $
                   R.defineTopRef0_ pq
                 invokeLIO ps $
                   R.modifyRef0 pq PQ.dequeue
                 invokeLIO ps $
                   c2 p2
                 call q p ps

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: Bool -> Dynamics LIO ()
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    LIO $ \ps ->
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- invokeLIO ps $
             invokeEvent p $
             R.readRef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeLIO ps $
              invokeDynamics p $
              processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent :: Dynamics LIO ()
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier :: Dynamics LIO ()
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore :: Dynamics LIO ()
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore :: Dynamics LIO ()
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: EventProcessing -> Dynamics LIO ()
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | Initialize the event queue in the current lattice node if required.
initEventQueue :: Event LIO ()
initEventQueue =
  Event $ \p ->
  LIO $ \ps ->
  do let pq = queuePQ $ runEventQueue r
         r  = pointRun p
     f <- invokeLIO ps $
          R.topRefDefined0 pq
     unless f $
       do case parentLIOParams ps of
            Nothing  -> error "The root must be initialized: initEventQueue"
            Just ps' ->
              do p' <- invokeLIO ps' $
                       invokeParameter r
                       latticePoint
                 invokeLIO ps' $
                   invokeEvent p'
                   initEventQueue
          invokeLIO ps $
            R.defineTopRef0_ pq
          invokeLIO ps $
            invokeDynamics p $
            processPendingEventsUnsafe True

-- | Estimate the specified reference.
estimateRef :: R.Ref a -> Estimate LIO a
estimateRef r =
  Estimate $ \p ->
  LIO $ \ps ->
  do invokeLIO ps $
       invokeEvent p
       initEventQueue
     invokeLIO ps $
       R.readRef0 r
