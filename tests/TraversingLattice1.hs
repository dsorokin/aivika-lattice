
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
        
model :: Simulation LIO ()
model =
  do let showLatticeNode :: Event LIO ()
         showLatticeNode =
           do t <- liftDynamics time
              i <- liftComp latticeTimeIndex
              k <- liftComp latticeMemberIndex
              liftIO $
                do putStr $ "t = " ++ show t
                   putStr $ ", time index = " ++ show i
                   putStr $ ", member index = " ++ show k
                   putStrLn ""
                   
     runEventInStartTime $
       enqueueEventWithIntegTimes
       showLatticeNode

     runEventInStopTime $
       return ()

main :: IO ()
main =
  do lattice <- newRandomLattice 5
     runLIO lattice $
       runSimulation model specs
