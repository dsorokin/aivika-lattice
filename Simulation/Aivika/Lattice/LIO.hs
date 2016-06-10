
-- |
-- Module     : Simulation.Aivika.Branch.LIO
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines 'LIO' as an instance of the 'MonadDES' and 'EventIOQueueing' type classes.
--
module Simulation.Aivika.Lattice.LIO
       (LIO,
        runLIO,
        latticeTimeIndex,
        latticeMemberIndex,
        latticeSize) where

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.QueueStrategy

import Simulation.Aivika.Lattice.Internal.LIO
import Simulation.Aivika.Lattice.Event
import Simulation.Aivika.Lattice.Generator
import Simulation.Aivika.Lattice.Ref.Base
import Simulation.Aivika.Lattice.QueueStrategy

instance MonadDES LIO

instance MonadComp LIO

-- | An implementation of the 'EventIOQueueing' type class.
instance EventIOQueueing LIO where

  enqueueEventIO = enqueueEvent
  
