
{-# LANGUAGE BangPatterns #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Internal.Ref
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The implementation of mutable references.
--
module Simulation.Aivika.Lattice.Internal.Ref
       (Ref,
        newEmptyRef,
        newEmptyRef0,
        newRef,
        newRef0,
        readRef,
        writeRef,
        modifyRef) where

-- import Debug.Trace

import Data.IORef
import qualified Data.IntMap as M

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Lattice.Internal.LIO

-- | A reference map.
type RefMap a = IORef (M.IntMap (IORef a))

-- | A mutable reference.
newtype Ref a = Ref { refMap :: RefMap a
                      -- ^ the map of actual references
                    }

instance Eq (Ref a) where
  r1 == r2 = (refMap r1) == (refMap r2)

-- | Return the map index.
lioMapIndex :: LIOParams -> Int
lioMapIndex ps = (lioTimeIndex ps) * (lioSize ps) + (lioMemberIndex ps)

-- | Create an empty reference.
newEmptyRef :: Simulation LIO (Ref a)
newEmptyRef = Simulation $ const newEmptyRef0

-- | Create an empty reference.
newEmptyRef0 :: LIO (Ref a)
newEmptyRef0 =
  LIO $ \ps ->
  do rm <- newIORef M.empty
     return Ref { refMap = rm }

-- | Create a new reference.
newRef :: a -> Simulation LIO (Ref a)
newRef = Simulation . const . newRef0

-- | Create a new reference.
newRef0 :: a -> LIO (Ref a)
newRef0 a =
  LIO $ \ps ->
  do r  <- invokeLIO ps newEmptyRef0
     ra <- newIORef a
     let !i = lioMapIndex ps
     writeIORef (refMap r) $
       M.insert i ra M.empty
     return r
     
-- | Read the value of a reference.
readRef :: Ref a -> Event LIO a
readRef r =
  Event $ \p ->
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let loop ps =
           case M.lookup (lioMapIndex ps) m of
             Just ra -> readIORef ra
             Nothing ->
               case parentLIOParams ps of
                 Just ps' -> loop ps'
                 Nothing  -> error "Cannot find branch: readRef"
     loop ps

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event LIO ()
writeRef r a =
  Event $ \p ->
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let !i = lioMapIndex ps
     case M.lookup i m of
       Just ra -> a `seq` writeIORef ra a
       Nothing ->
         do ra <- a `seq` newIORef a
            modifyIORef (refMap r) $ M.insert i ra

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event LIO ()
modifyRef r f =
  Event $ \p ->
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let !i = lioMapIndex ps
     case M.lookup i m of
       Just ra ->
         do a <- readIORef ra
            let b = f a
            b `seq` writeIORef ra b
       Nothing ->
         do a <- invokeLIO ps $ invokeEvent p $ readRef r
            invokeLIO ps $ invokeEvent p $ writeRef r (f a)
