
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
-- Also it defines basic functions for running nested computations within lattice nodes.
--
module Simulation.Aivika.Lattice.Event
       (estimateEvent,
        estimateEventByTime) where

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
  do let pq = queuePQ $ runEventQueue $ pointRun p
     f <- invokeLIO ps $
          R.topRefDefined0 pq
     unless f $
       do case parentLIOParams ps of
            Nothing  -> error "The root must be initialized: initEventQueue"
            Just ps' ->
              do p' <- invokeLIO ps' $
                       invokeEvent p
                       latticePoint
                 invokeLIO ps' $
                   invokeEvent p'
                   initEventQueue
          invokeLIO ps $
            R.defineTopRef0_ pq
          invokeLIO ps $
            invokeDynamics p $
            processPendingEventsUnsafe True

-- | Return the point in the corresponding lattice node.
latticePoint :: Event LIO (Point LIO)
latticePoint =
  Event $ \p ->
  do let r = pointRun p
     t <- invokeParameter r latticeTime
     let sc = pointSpecs p
         t0 = spcStartTime sc
         dt = spcDT sc
         n  = fromIntegral $ floor ((t - t0) / dt)
     return $ p { pointTime = t,
                  pointIteration = n,
                  pointPhase = -1 }

-- | Return the computation value in the corresponding lattice node.
latticeEvent :: Event LIO a -> Event LIO a
latticeEvent m =
  Event $ \p ->
  LIO $ \ps ->
  do p' <- invokeLIO ps $
           invokeEvent p $
           latticePoint
     invokeLIO ps $
       invokeEvent p' $
       initEventQueue
     invokeLIO ps $
       invokeEvent p' m

-- | Run the specified nested computation in the next nodes of the lattice using
-- the integration time points for calculating the 'latticeTimeIndex',
-- which will be increased by one. The first result will correspond to
-- the current 'latticeMemberIndex'. The second result will correspond to
-- the @latticeMemberIndex@ increased by one.
--
-- The current computation within the current lattice node remains intact.
-- Nevertheless, this function has the following important side effect,
-- which is an essence of the lattice approach at the same time.
--
-- The event queue state is memoized in the nodes of the lattice. Each node
-- is defined by a pair of @latticeTimeIndex@ and @latticeMemberIndex@. It means
-- that when calling @nextEvents@ next time, we can return one of the results immediately
-- without traversing the event queue if the corresponding node was traversed before.
--
nextEvents :: Event LIO a -> Event LIO (Event LIO a, Event LIO a)
nextEvents m =
  Event $ \p ->
  LIO $ \ps ->
  do (ps1, ps2) <- invokeLIO ps $
                   invokeEvent p
                   nextLIOParams
     let m1 =
           Event $ \p ->
           LIO $ \ps ->
           invokeLIO ps1 $
           invokeEvent p $
           latticeEvent m
         m2 =
           Event $ \p ->
           LIO $ \ps ->
           invokeLIO ps2 $
           invokeEvent p $
           latticeEvent m
     return (m1, m2)

-- | Estimate the event computation in the lattice nodes by the specified time.
estimateEventByTime :: (Event LIO a -> Event LIO a -> Event LIO a)
                       -- ^ reduce in the intermediate time point of the lattice
                       -> Event LIO a
                       -- ^ estimate in the final time point of the lattice
                       -> Simulation LIO (Double -> Event LIO a)
estimateEventByTime f m =
  do r  <- R.newRef Nothing
     t2 <- liftParameter stoptime
     let loop =
           do b <- R.readRef r
              case b of
                Just a  -> return a
                Nothing ->
                  do t  <- liftDynamics time
                     if t >= t2
                       then do a <- m
                               a `seq` R.writeRef r (Just a)
                               return a
                       else do (m1, m2) <- nextEvents loop
                               a <- f m1 m2
                               a `seq` R.writeRef r (Just a)
                               return a
     return $ \t ->
       Event $ \p ->
       LIO $ \ps ->
       do ps' <- invokeLIO ps $
                 invokeParameter (pointRun p) $
                 projectLIOParams t
          invokeLIO ps' $
            invokeEvent p
            loop

-- | Estimate the event computation in the lattice nodes by the current time.
estimateEvent :: (Event LIO a -> Event LIO a -> Event LIO a)
                 -- ^ reduce in the intermediate time point of the lattice
                 -> Event LIO a
                 -- ^ estimate in the final time point of the lattice
                 -> Simulation LIO (Event LIO a)
estimateEvent f m =
  do e <- estimateEventByTime f m
     return $
       Event $ \p ->
       invokeEvent p $
       e (pointTime p)
