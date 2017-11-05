
-- |
-- Module     : Simulation.Aivika.Lattice.Internal.Lattice
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines the lattice.
--
module Simulation.Aivika.Lattice.Internal.Lattice
       (LIOLattice(..),
        lattice,
        newRandomLattice,
        newRandomLatticeWithProb) where

import Control.Monad
import Control.Monad.Trans

import Data.Array

import qualified System.Random.MWC as MWC

-- | Specifies the lattice.
data LIOLattice =
  LIOLattice { lioParentMemberIndex :: Int -> Int -> Int,
               -- ^ Get the parent member index by the specified
               -- time and member indices.
               lioSize :: Int
               -- ^ Tha lattice size.
             }

-- | Create a new random lattice by the specified probability and size,
-- where the probability defines whether the interior child node derives
-- from the right parent.
newRandomLatticeWithProb :: Double -> Int -> IO LIOLattice
newRandomLatticeWithProb p m =
  do g <- MWC.withSystemRandom (return :: MWC.GenIO -> IO MWC.GenIO)
     xss0 <- forM [0 .. m] $ \i ->
       do xs0 <- forM [0 .. i] $ \k ->
            if k == 0
            then return k
            else if k == i
                 then return (k - 1)
                 else do x <- MWC.uniform g :: IO Double
                         if x > p
                           then return (k - 1)
                           else return k
          return $ listArray (0, i) xs0
     let xss = listArray (0, m) xss0
     return LIOLattice { lioParentMemberIndex = \i k -> (xss ! i) ! k,
                         lioSize = m
                       }

-- | Create a new random lattice by the specified size with equal probabilities,
-- whether the interior child node derives from the left or right parents.
newRandomLattice :: Int -> IO LIOLattice
newRandomLattice = newRandomLatticeWithProb 0.5

-- | Return a lattice by the specifed size and the parent member function.
lattice :: Int
           -- ^ the lattice size
           -> (Int -> Int -> Int)
           -- ^ get the parent member index by the specified
           -- time and member indices
           -> LIOLattice
lattice m f = LIOLattice f m

