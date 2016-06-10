
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Event
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The module defines an event queue, where 'LIO' is an instance of 'EventQueueing'.
-- Also it defines basic functions for branching computations.
--
module Simulation.Aivika.Lattice.Event () where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import qualified Simulation.Aivika.PriorityQueue.Pure as PQ

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Lattice.Internal.LIO
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
    do invokeDynamics p $ processEvents processing
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
                 call q p p ps
                 writeIORef f False
  call q p p0 ps0 =
    do let pq = queuePQ q
           r  = pointRun p
       f <- invokeLIO ps0 $
            invokeEvent p0 $
            fmap PQ.queueNull $ R.readRef pq
       unless f $
         do (t2, c2) <- invokeLIO ps0 $
                        invokeEvent p0 $
                        fmap PQ.queueFront $ R.readRef pq
            let t = queueTime q
            t' <- invokeLIO ps0 $
                  invokeEvent p0 $
                  R.readRef t
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
                 ps2 <- invokeLIO ps0 $
                        invokeEvent p2
                        bestSuitedLIOParams
                 invokeLIO ps2 $
                   invokeEvent p2 $
                   R.writeRef t t2
                 invokeLIO ps2 $
                   invokeEvent p2 $
                   R.modifyRef pq PQ.dequeue
                 invokeLIO ps2 $
                   c2 p2
                 call q p p2 ps2

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
