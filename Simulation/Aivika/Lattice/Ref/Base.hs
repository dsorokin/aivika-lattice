
-- |
-- Module     : Simulation.Aivika.Lattice.Ref.Base
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- Here is an implementation of strict mutable references, where
-- 'LIO' is an instance of 'MonadRef' and 'MonadRef0'.
--
module Simulation.Aivika.Lattice.Ref.Base
       (module Simulation.Aivika.Lattice.Ref.Base.Strict) where

import Simulation.Aivika.Trans.Ref.Base

import Simulation.Aivika.Lattice.Internal.LIO
import Simulation.Aivika.Lattice.Ref.Base.Strict
