
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Lattice
import Simulation.Aivika.Experiment.Histogram

meanUpTime = 1.0
meanRepairTime = 0.5

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation LIO Histogram
model =
  do let latticeDistribution :: Estimate LIO [Int]
         latticeDistribution =
           do k <- liftComp latticeMemberIndex
              return [k]

     let reduce :: [Int] -> [Int] -> Estimate LIO [Int]
         reduce ks1 ks2 = return $ ks1 ++ ks2 
 
     let leaf = latticeDistribution

     ks <- foldEstimate reduce leaf

     runEstimateInStartTime $
       do xs <- ks
          let ys = fmap (fromIntegral) xs
              hs = histogram binScott [ys]
          return hs

main :: IO ()
main =
  do lat <- newRandomLattice 10
     hs <- runLIO lat $
           runSimulation model specs
     putStrLn "Histogram:"
     putStrLn (show hs)
