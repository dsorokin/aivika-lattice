
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
module Simulation.Aivika.Lattice.Event () where

import Simulation.Aivika.Lattice.Internal.Event
