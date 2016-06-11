
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
                   
     runEventInStartTime $
       enqueueEventWithIntegTimes $
       showLatticeNode "enqueue"

     let traverse m1 m2 =
           do showLatticeNode "traverse"
              m1
              m2
              return ()

     let leaf = showLatticeNode "leaf"

     e <- estimateEvent traverse leaf

     runEventInStartTime
       e

     runEventInStopTime $
       showLatticeNode "stop"

main :: IO ()
main =
  runLIO $
  runSimulation model specs
