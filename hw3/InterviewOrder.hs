-- InterviewOrder.hs

import Control.Monad (replicateM)
import System.Random (newStdGen, mkStdGen)
import System.Random.Shuffle (shuffle')
import System.Environment (getArgs)
import Data.List.Split (chunksOf)
import Data.List (elemIndices, findIndices)
import Data.Maybe (fromMaybe)

isSuccess :: Int -> [Int] -> Bool
isSuccess k xs = 1 `notElem` take (k-1) xs
                 && head (elemIndices 1 xs) <= head (findIndices
                                                     (< minimum (take (k-1) xs))
                                                     (drop (k-1) xs)) + k - 1

main :: IO ()
main = do
  args <- getArgs
  let n = read $ head args :: Int
      k = minimum [x | x <- [1..n-1], sum [1 / fromIntegral y | y <- [x..n-1]] <= 1]
      iters = read $ args!!1 
  gens <- replicateM iters newStdGen
  let randomOrders = map (shuffle' [1..n] n) gens :: [[Int]]
      numSuccesses = length $ filter (== True) (map (isSuccess k) randomOrders) 
      prob = fromIntegral numSuccesses / fromIntegral iters :: Double
  putStrLn $ "We found a probability of " ++ show prob
      
