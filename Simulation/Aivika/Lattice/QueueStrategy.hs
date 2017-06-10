
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Lattice.QueueStrategy
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines queue strategies 'FCFS' and 'LCFS' for the 'LIO' computation.
--
module Simulation.Aivika.Lattice.QueueStrategy () where

import Control.Monad.Trans

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL

import Simulation.Aivika.Lattice.Internal.LIO
import Simulation.Aivika.Lattice.Ref.Base

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy LIO FCFS where

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue LIO FCFS a = FCFSQueueLIO (LL.DoubleLinkedList LIO a)

  newStrategyQueue s = fmap FCFSQueueLIO LL.newList

  strategyQueueNull (FCFSQueueLIO q) = LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy LIO FCFS where

  strategyDequeue (FCFSQueueLIO q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance EnqueueStrategy LIO FCFS where

  strategyEnqueue (FCFSQueueLIO q) i = LL.listAddLast q i

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy LIO LCFS where

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue LIO LCFS a = LCFSQueueLIO (LL.DoubleLinkedList LIO a)

  newStrategyQueue s = fmap LCFSQueueLIO LL.newList
       
  strategyQueueNull (LCFSQueueLIO q) = LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy LIO LCFS where

  strategyDequeue (LCFSQueueLIO q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance EnqueueStrategy LIO LCFS where

  strategyEnqueue (LCFSQueueLIO q) i = LL.listInsertFirst q i
