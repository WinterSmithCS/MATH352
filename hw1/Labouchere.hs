-- file Labouchere.hs
-- A program to simulate the Labouchere system

import Control.Monad (replicateM)
import System.Random (randomR, newStdGen, StdGen)
import System.Environment (getArgs)

labouchere :: Num a => [a] -> StdGen -> ([a], a)
labouchere xs = labouchere' xs 0

labouchere' :: Num a => [a] -> a -> StdGen -> ([a], a)
labouchere' xs acc gen
  | null xs               = (xs, acc)
  | randNumber `elem` red = labouchere' winXs winAcc newGen
  | otherwise             = labouchere' loseXs loseAcc newGen
  where (randNumber, newGen) = randomR (0,37) gen :: (Int, StdGen)
        (winXs, loseXs, winAcc, loseAcc)
          | length xs > 1 = ( init $ tail xs
                            , xs ++ [head xs + last xs]
                            , acc + head xs + last xs
                            , acc - head xs - last xs
                            )
          | otherwise     = ( []
                            , xs ++ [head xs]
                            , acc + head xs
                            , acc - head xs
                            )

red = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]

main :: IO ()
main = do
  args <- getArgs
  let iters = read $ head args
      list = read $ args!!1 :: [Double]
  gens <- replicateM iters newStdGen
  let results = map (labouchere list) gens
  print $
    if all (\(x,y) -> null x && y == sum list) results
    then "All returned lists are empty and returned the sum of the list"
    else "The impossible has occurred"
