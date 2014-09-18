-- MathConference.hs

import Control.Monad (replicateM)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import System.Environment (getArgs)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph (mkGraph, Node, edges)
import Data.Graph.Inductive.Basic (undir)

seatingArrangement :: (Enum a, Enum b, Eq b, Num a, Num b) => [Node] -> Gr a b
seatingArrangement vs = undir $ mkGraph lnodes ledges
  where ledges = take (length vs) $ zip3 (cycle vs) (tail $ cycle vs) [1..]
        lnodes = take (length vs) $ zip vs [1..]

noProfessorsAdjacentTwice :: (Eq a) => Gr a b -> Gr a b -> Bool
noProfessorsAdjacentTwice xs ys = null [ x | x <- edges xs, y <- edges ys, x == y ]

main :: IO ()
main = do
  args <- getArgs
  let (n, iters) = (read (args!!0)::Int, read (args!!1)::Int)
  gens <- replicateM (2*iters) newStdGen
  let lists = map (shuffle' [1..n] n) gens :: [[Int]]
      arrangementGraphs = map seatingArrangement lists
      pairedMeals = splitAt iters arrangementGraphs
      sims = length $ filter (== True) $ uncurry (zipWith noProfessorsAdjacentTwice) pairedMeals
      prob = fromIntegral sims / fromIntegral iters
  putStrLn $ "We estimate a " ++ show prob ++ " probability."

--  LocalWords:  MathConference hs
