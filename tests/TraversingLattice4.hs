
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Lattice

meanUpTime = 1.0
meanRepairTime = 0.5

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 400.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation LIO ()
model =
  do let showLatticeNode :: String -> Event LIO ()
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

     r <- newRef 0
                   
     runEventInStartTime $
       enqueueEventWithIntegTimes $
       do x <- liftParameter $ randomUniform 0 1
          writeRef r x
          showLatticeNode ("enqueue (x = " ++ show x ++ ")") 

     let reduce :: Double -> Double -> Estimate LIO Double
         reduce a b =
           do let x = (a + b) / 2
              traceEstimate ("reduce (x = " ++ show x ++ ")") $
                return x

     let leaf =
           do x <- readObservable r
              traceEstimate ("leaf (x = " ++ show x ++ ")") $
                return x

     m <- foldEstimate reduce leaf

     runEstimateInStartTime $
       do x <- m
          traceEstimate ("result (x = " ++ show x ++ ")") $
            return ()

     runEventInStopTime $
       showLatticeNode "stop"

main :: IO ()
main =
  runLIO $
  runSimulation model specs
