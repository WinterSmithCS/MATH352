-- ex25c.hs

import Control.Monad (replicateM)
import System.Random (newStdGen, StdGen, randoms, random)
import System.Environment (getArgs)

-- | A 'trial' is a function that takes a random generator and returns a
-- list of 20 random 0s and 1s.
trial :: StdGen -> [Bool]
trial g = take 20 $ randoms g

-- | 'numSuccesses' takes two lists of 20 booleans and returns the
-- number of values that are the same at each index.
numSuccesses :: [Bool] -> [Bool] -> Int
numSuccesses g t = length . filter (== True) $ zipWith (==) g t

-- | 'guess' iterates a list of 10 true and 10 false booleans and
-- attempts to guess the value of the next boolean. The list returned
-- is a list of each guess, _not_ the success or failure of each guess.
guess :: [Bool] -> StdGen -> [Bool]
guess = iterGuesses 0 0 []

iterGuesses numTrue numFalse acc [] gen = acc
iterGuesses numTrue numFalse acc (x:xs) gen =
  iterGuesses newTrue newFalse (acc ++ [newGuess]) xs newGen
  where (newGuess, newGen) = nextGuess numTrue numFalse gen
        (newTrue, newFalse)
          | x         = ( numTrue + 1
                        , numFalse
                        )
          | otherwise = ( numTrue
                        , numFalse + 1
                        )


nextGuess :: Int -> Int -> StdGen -> (Bool, StdGen)
nextGuess numTrue numFalse gen
  | numTrue < numFalse = (True, gen)
  | numTrue > numFalse = (False, gen)
  | otherwise = random gen

main :: IO ()
main = do
  args <- getArgs

  -- The first argument is the number of iterations
  let iters = read $ head args

  -- For each iteration, we'll need a random generator for the actual
  -- sequence of cards
  trialGens <- replicateM iters newStdGen
  guessGens <- replicateM iters newStdGen

  -- Since our guesses are without information, we can gather them
  -- before we even draw cards.
  let trials = map trial trialGens
      guesses = zipWith guess trials guessGens
      values = zipWith numSuccesses guesses trials
      expectedValue = fromIntegral (sum values) / fromIntegral iters

  putStrLn $ "We estimate the expected value to be " ++ show expectedValue
    ++ "."

--  LocalWords: hs
