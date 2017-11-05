
{-
  A binomial option pricing model

  Assume a put option with strike price $110 currently trading at $100 and
  expiring in one year. Annual risk free rate is at 5%. Price is expected
  to increase 20% and decrease 15% every six months. It is necessary to estimate
  the price of the put option.
 -}

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Lattice
import Simulation.Aivika.Experiment.Histogram

-- the lattice size
n = 50

-- the up and down factors
u0 = 1.2
d0 = 0.85

-- corrected factors for the lattice size
u = exp (log u0 / (fromIntegral n / 2))
d = exp (log d0 / (fromIntegral n / 2))

-- initial stock price
s0 = 100.0

-- strike price for put option
strikePrice = 110.0

-- risk free rate
r = 0.05

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1.0,
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
