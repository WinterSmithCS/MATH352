-- HolmesWatson.hs

import Control.Monad (replicateM)
import System.Random (newStdGen, randomRs, RandomGen, StdGen, mkStdGen)
import System.Random.Shuffle (shuffle')
import System.Environment (getArgs)
import Data.List.Split (chunksOf)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x1, x2) = (f x1, f x2)

isCloser :: (Ord a, Num a) => a -> [[a]] -> [[a]] -> [Bool]
isCloser n = zipWith (\xs ys -> abs (2*maximum xs - n) < abs (maximum ys - n))

main :: IO ()
main = do
  args <- getArgs
  gen <- newStdGen
  let bigN = read $ head args :: Int
      iters = read $ args!!1
      predictions = take (iters*2*16) $ randomRs (1, bigN) gen
      (holmes, watson) = mapTuple (chunksOf 16) $ splitAt (iters*16) predictions
      numHolmesCloser = length $ filter (== True) $ isCloser bigN holmes watson
  putStrLn $ "Over " ++ show iters ++ " iterations, we found the proportion of instances"
    ++ " Holmes was closer to be "
    ++ show (fromIntegral numHolmesCloser / fromIntegral iters) ++ "."
