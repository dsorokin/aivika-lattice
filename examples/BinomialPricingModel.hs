
-- A binomial options pricing model

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Lattice
import Simulation.Aivika.Experiment.Histogram

-- the up and down factors
u = 1.2
d = 0.85

-- initial stock price
s0 = 100.0

-- strike price for put option
strikePrice = 110.0

-- risk free rate
r = 0.05

-- the lattice size (defines the time of execution)
n = 100

specs = Specs { spcStartTime = 0.0,
                spcStopTime = fromIntegral n * 0.5,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation LIO Double
model =
  do -- stock price
     s <- newRef s0

     -- calculate the stock price tree
     runEventInStartTime $
       enqueueEventWithLatticeTimes $
       do k  <- liftComp latticeMemberIndex
          k0 <- liftComp latticeParentMemberIndex
          case k0 of
            Nothing -> return ()
            Just k0 | k == k0 ->
              modifyRef s (\x -> x * u)
            Just k0 | k == k0 + 1 ->
              modifyRef s (\x -> x * d)

     -- the lattice time step
     dt <- liftParameter latticeTimeStep

     -- calculate the up move probability
     let p = (exp (- r * dt) - d) / (u - d)

     -- estimate the option price in the end time
     let leaf :: Estimate LIO Double
         leaf =
           do x <- readObservable s
              -- this is a put option
              return $ max (strikePrice - x) 0

     -- estimate the option price by the forecast
     let reduce :: Double -> Double -> Estimate LIO Double
         reduce x1 x2 =
           return $
           exp (- r * dt) * (p * x1 + (1 - p) * x2)

     price <- foldEstimate reduce leaf

     runEstimateInStartTime price
 
main :: IO ()
main =
  do lat <- newRandomLattice n
     e <- runLIO lat $
          runSimulation model specs
     putStrLn "Estimation:"
     putStrLn (show e)
