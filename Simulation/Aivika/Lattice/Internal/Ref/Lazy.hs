
{-# LANGUAGE BangPatterns #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Internal.Ref.Lazy
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The implementation of lazy mutable references.
--
module Simulation.Aivika.Lattice.Internal.Ref.Lazy
       (Ref,
        newEmptyRef,
        newEmptyRef0,
        newRef,
        newRef0,
        readRef,
        readRef0,
        writeRef,
        writeRef0,
        modifyRef,
        modifyRef0,
        topRefDefined0,
        defineTopRef0,
        defineTopRef0_) where

-- import Debug.Trace

import Data.IORef
import qualified Data.IntMap as M

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Lattice.Internal.LIO

-- | A reference map.
type RefMap a = IORef (M.IntMap (IORef a))

-- | A lazy mutable reference.
newtype Ref a = Ref { refMap :: RefMap a
                      -- ^ the map of actual references
                    }

instance Eq (Ref a) where
  r1 == r2 = (refMap r1) == (refMap r2)

-- | Return the map index.
lioMapIndex :: LIOParams -> Int
lioMapIndex ps = ((i * (i + 1)) `div` 2) + k
  where i = lioTimeIndex ps
        k = lioMemberIndex ps

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
readRef = Event . const . readRef0
     
-- | Read the value of a reference.
readRef0 :: Ref a -> LIO a
readRef0 r =
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let loop ps =
           case M.lookup (lioMapIndex ps) m of
             Just ra -> readIORef ra
             Nothing ->
               case parentLIOParams ps of
                 Just ps' -> loop ps'
                 Nothing  -> error "Cannot find lattice node: readRef0"
     loop ps

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event LIO ()
writeRef r a = Event $ const $ writeRef0 r a 

-- | Write a new value into the reference.
writeRef0 :: Ref a -> a -> LIO ()
writeRef0 r a =
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let !i = lioMapIndex ps
     case M.lookup i m of
       Just ra -> writeIORef ra a
       Nothing ->
         do ra <- newIORef a
            modifyIORef (refMap r) $ M.insert i ra

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event LIO ()
modifyRef r f = Event $ const $ modifyRef0 r f

-- | Mutate the contents of the reference.
modifyRef0 :: Ref a -> (a -> a) -> LIO ()
modifyRef0 r f =
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let !i = lioMapIndex ps
     case M.lookup i m of
       Just ra ->
         do a <- readIORef ra
            let b = f a
            writeIORef ra b
       Nothing ->
         do a <- invokeLIO ps $ readRef0 r
            invokeLIO ps $ writeRef0 r (f a)

-- | Whether the top reference value is defined.
topRefDefined0 :: Ref a -> LIO Bool
topRefDefined0 r =
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let !i = lioMapIndex ps
     case M.lookup i m of
       Just ra -> return True
       Nothing -> return False

-- | Define the top reference value.
defineTopRef0 :: Ref a -> LIO a
defineTopRef0 r =
  LIO $ \ps ->
  do m <- readIORef (refMap r)
     let !i = lioMapIndex ps
     case M.lookup i m of
       Just ra -> readIORef ra
       Nothing ->
         case parentLIOParams ps of
           Nothing  -> error "Cannot find parent: defineTopRef0"
           Just ps' ->
             do a  <- invokeLIO ps' $ defineTopRef0 r
                ra <- newIORef a
                modifyIORef (refMap r) $ M.insert i ra
                return a

-- | Ensure that the top reference value is defined.
defineTopRef0_ :: Ref a -> LIO ()
defineTopRef0_ r =
  do a <- defineTopRef0 r
     return ()
