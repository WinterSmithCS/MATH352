-- ex25a.hs

import Control.Monad (replicateM)
import System.Random (newStdGen, StdGen, randoms)
import System.Random.Shuffle (shuffle')
import System.Environment (getArgs)

-- | A 'trial' is a function that takes a random generator and returns a
-- list of 20 random 0s and 1s.
trial :: StdGen -> [Bool]
trial = shuffle' list 20
  where list = replicate 10 True ++ replicate 10 False

-- | 'numSuccesses' takes two lists of 20 booleans and returns the
-- number of values that are the same at each index.
numSuccesses :: [Bool] -> [Bool] -> Int
numSuccesses g t = length . filter (== True) $ zipWith (==) g t

main :: IO ()
main = do
  args <- getArgs

  -- The first argument is the number of iterations
  let iters = read $ head args

  -- For each iteration, we'll need a random generator for our guesses
  -- and a random generator for the actual sequence of cards
  guessGens <- replicateM iters newStdGen
  trialGens <- replicateM iters newStdGen

  -- Since our guesses are without information, we can gather them
  -- before we even draw cards.
  let guesses = map trial guessGens
      trials = map trial trialGens
      values = zipWith numSuccesses guesses trials
      expectedValue = fromIntegral (sum values) / fromIntegral iters

  putStrLn $ "We estimate the expected value to be " ++ show expectedValue
    ++ "."

--  LocalWords: hs
