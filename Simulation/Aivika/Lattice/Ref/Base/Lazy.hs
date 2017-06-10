
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Ref.Base
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- Here is an implementation of lazy mutable references, where
-- 'LIO' is an instance of 'MonadRef' and 'MonadRef0'.
--
module Simulation.Aivika.Lattice.Ref.Base.Lazy () where

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Ref.Base.Lazy
import Simulation.Aivika.Trans.Observable

import Simulation.Aivika.Lattice.Internal.LIO
import qualified Simulation.Aivika.Lattice.Internal.Ref.Lazy as R
import Simulation.Aivika.Lattice.Internal.Estimate
import Simulation.Aivika.Lattice.Internal.Event

-- | The implementation of mutable references.
instance MonadRef LIO where

  -- | The mutable reference.
  newtype Ref LIO a = Ref { refValue :: R.Ref a }

  {-# INLINE newRef #-}
  newRef = fmap Ref . R.newRef 

  {-# INLINE readRef #-}
  readRef (Ref r) = R.readRef r

  {-# INLINE writeRef #-}
  writeRef (Ref r) = R.writeRef r

  {-# INLINE modifyRef #-}
  modifyRef (Ref r) = R.modifyRef r

  {-# INLINE equalRef #-}
  equalRef (Ref r1) (Ref r2) = (r1 == r2)

-- | A subtype of mutable references that can be created under more weak conditions.
instance MonadRef0 LIO where

  {-# INLINE newRef0 #-}
  newRef0 = fmap Ref . R.newRef0

-- | An instance of the specified type class.
instance Observable (Ref LIO) (Estimate LIO) where

  {-# INLINE readObservable #-}
  readObservable (Ref r) = estimateLazyRef r
