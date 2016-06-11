
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
        
model :: Simulation LIO Double
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

     let showLatticeNode :: String -> Event LIO ()
         showLatticeNode action =
           do t <- liftDynamics time
              i <- liftComp latticeTimeIndex
              k <- liftComp latticeMemberIndex
              liftIO $
                do putStr action
                   putStr $ ": t = " ++ show t
                   putStr $ ", time index = " ++ show i
                   putStr $ ", member index = " ++ show k
                   putStrLn ""

     let leaf =
           do x <- readRef totalUpTime
              t <- liftDynamics time
              let a = x / (2 * t) 
              showLatticeNode $ "leaf (" ++ show a ++ ")" 
              a `seq` return a

     let reduce m1 m2 =
           do a1 <- m1
              a2 <- m2
              let a = (a1 + a2) / 2
              showLatticeNode $ "result (" ++ show a ++ ")" 
              a `seq` return a

     r <- estimateEvent reduce leaf

     runEventInStartTime $
       do showLatticeNode "launch"
          r

main :: IO ()
main =
  do a <- runLIO $
          runSimulation model specs
     print a
