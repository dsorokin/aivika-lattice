
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Lattice.Generator
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- Here is defined a random number generator,
-- where 'LIO' is an instance of 'MonadGenerator'.
--
module Simulation.Aivika.Lattice.Generator () where

import Control.Monad
import Control.Monad.Trans

import System.Random
import qualified System.Random.MWC as MWC

import Data.IORef

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Generator.Primitive
import Simulation.Aivika.Lattice.Internal.LIO

instance MonadGenerator LIO where

  data Generator LIO =
    GeneratorLIO { generator01 :: LIO Double,
                   -- ^ the generator of uniform numbers from 0 to 1
                   generatorNormal01 :: LIO Double,
                   -- ^ the generator of normal numbers with mean 0 and variance 1
                   generatorSequenceNo :: LIO Int
                   -- ^ the generator of sequence numbers
                 }

  generateUniform = generateUniform01 . generator01

  generateUniformInt = generateUniformInt01 . generator01

  generateTriangular = generateTriangular01 . generator01

  generateNormal = generateNormal01 . generatorNormal01

  generateLogNormal = generateLogNormal01 . generatorNormal01

  generateExponential = generateExponential01 . generator01

  generateErlang = generateErlang01 . generator01

  generatePoisson = generatePoisson01 . generator01

  generateBinomial = generateBinomial01 . generator01

  generateGamma g = generateGamma01 (generatorNormal01 g) (generator01 g)

  generateBeta g = generateBeta01 (generatorNormal01 g) (generator01 g)

  generateWeibull = generateWeibull01 . generator01

  generateDiscrete = generateDiscrete01 . generator01

  generateSequenceNo = generatorSequenceNo

  newGenerator tp =
    case tp of
      SimpleGenerator ->
        do let g = MWC.uniform <$>
                   MWC.createSystemRandom
           g' <- liftIO g
           newRandomGenerator01 (liftIO g')
      SimpleGeneratorWithSeed x ->
        error "Unsupported generator type SimpleGeneratorWithSeed: newGenerator"
      CustomGenerator g ->
        g
      CustomGenerator01 g ->
        newRandomGenerator01 g

  newRandomGenerator g = 
    do r <- liftIO $ newIORef g
       let g01 = do g <- liftIO $ readIORef r
                    let (x, g') = random g
                    liftIO $ writeIORef r g'
                    return x
       newRandomGenerator01 g01

  newRandomGenerator01 g01 =
    do gNormal01 <- newNormalGenerator01 g01
       gSeqNoRef <- liftIO $ newIORef 0
       let gSeqNo =
             do x <- liftIO $ readIORef gSeqNoRef
                liftIO $ modifyIORef' gSeqNoRef (+1)
                return x
       return GeneratorLIO { generator01 = g01,
                             generatorNormal01 = gNormal01,
                             generatorSequenceNo = gSeqNo }

-- | Create a normal random number generator with mean 0 and variance 1
-- by the specified generator of uniform random numbers from 0 to 1.
newNormalGenerator01 :: LIO Double
                        -- ^ the generator
                        -> LIO (LIO Double)
newNormalGenerator01 g =
  do nextRef <- liftIO $ newIORef 0.0
     flagRef <- liftIO $ newIORef False
     xi1Ref  <- liftIO $ newIORef 0.0
     xi2Ref  <- liftIO $ newIORef 0.0
     psiRef  <- liftIO $ newIORef 0.0
     let loop =
           do psi <- liftIO $ readIORef psiRef
              if (psi >= 1.0) || (psi == 0.0)
                then do g1 <- g
                        g2 <- g
                        let xi1 = 2.0 * g1 - 1.0
                            xi2 = 2.0 * g2 - 1.0
                            psi = xi1 * xi1 + xi2 * xi2
                        liftIO $ writeIORef xi1Ref xi1
                        liftIO $ writeIORef xi2Ref xi2
                        liftIO $ writeIORef psiRef psi
                        loop
                else liftIO $ writeIORef psiRef $ sqrt (- 2.0 * log psi / psi)
     return $
       do flag <- liftIO $ readIORef flagRef
          if flag
            then do liftIO $ writeIORef flagRef False
                    liftIO $ readIORef nextRef
            else do liftIO $ writeIORef xi1Ref 0.0
                    liftIO $ writeIORef xi2Ref 0.0
                    liftIO $ writeIORef psiRef 0.0
                    loop
                    xi1 <- liftIO $ readIORef xi1Ref
                    xi2 <- liftIO $ readIORef xi2Ref
                    psi <- liftIO $ readIORef psiRef
                    liftIO $ writeIORef flagRef True
                    liftIO $ writeIORef nextRef $ xi2 * psi
                    return $ xi1 * psi
