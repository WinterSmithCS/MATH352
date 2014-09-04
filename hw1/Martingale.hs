-- file: Martingale.hs
-- a program to simulate the martingale doubling system

import System.Random (randomR, newStdGen, StdGen)
import System.Environment (getArgs)
import Control.Monad (replicateM)

red = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]

martingale :: StdGen -> Bool
martingale = martingale' 1 0

martingale' :: Real a => a -> a -> StdGen -> Bool
martingale' bet acc gen
  | acc >= 5     = True
  | acc <= -100  = False
  | otherwise    = 
    let (randNumber, newGen) = randomR (0,37) gen :: (Int, StdGen)
    in if randNumber `elem` red
       then martingale' 1 (acc + bet) newGen
       else martingale' (bet * 2) (acc - bet) newGen

main :: IO ()
main = do
  args <- getArgs
  let iters = read $ head args
  gens <- replicateM iters newStdGen
  let results = map martingale gens
      countWins = length $ filter (== True) results
      prob = fromIntegral countWins / fromIntegral iters
  print prob
