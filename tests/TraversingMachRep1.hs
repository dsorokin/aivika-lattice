
-- It corresponds to model MachRep1 described in document 
-- Introduction to Discrete-Event Simulation and the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Two machines, which sometimes break down.
-- Up time is exponentially distributed with mean 1.0, and repair time is
-- exponentially distributed with mean 0.5. There are two repairpersons,
-- so the two machines can be repaired simultaneously if they are down
-- at the same time.
--
-- Output is long-run proportion of up time. Should get value of about
-- 0.66.

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Lattice

meanUpTime = 1.0
meanRepairTime = 0.5

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 100.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation LIO (Results LIO)
model =
  do totalUpTime <- newRef 0.0
     
     let machine =
           do upTime <-
                liftParameter $
                randomExponential meanUpTime
              --
              -- r <- liftSimulation $ newRef 10
              --
              holdProcess upTime
              liftEvent $ 
                modifyRef totalUpTime (+ upTime)
              repairTime <-
                liftParameter $
                randomExponential meanRepairTime
              holdProcess repairTime
              machine

     runProcessInStartTime machine
     runProcessInStartTime machine

     t2 <- liftParameter stoptime
     
     let forecastUpTimeProp :: Event LIO Double
         forecastUpTimeProp =
           do t <- liftDynamics time
              if t >= t2
                then do x <- readRef totalUpTime
                        return $ x / (2 * t)
                else do (x1, x2) <- nextEvents forecastUpTimeProp
                        let x = (x1 + x2) / 2
                        x `seq` return x

     v <- runEventInStartTime forecastUpTimeProp

     let upTimePropForecasted :: Event LIO Double
         upTimePropForecasted = return v
     
     let upTimeProp =
           do x <- readRef totalUpTime
              t <- liftDynamics time
              return $ x / (2 * t)

     return $
       results
       [resultSource
        "upTimeProp"
        "The long-run proportion of up time (~ 0.66)"
        upTimeProp,
        --
        resultSource
        "upTimePropForecasted"
        "The forecasted long-run proption of up time"
        upTimePropForecasted]

main :: IO ()
main =
  runLIO $
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
