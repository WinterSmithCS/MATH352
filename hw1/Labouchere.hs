-- file Labouchere.hs
-- A program to simulate the Labouchere system

import System.Random (randomR, getStdGen, StdGen)

labouchere :: Num a => [a] -> IO a
labouchere xs = do
  gen <- getStdGen
  labouchere' xs 0 gen

labouchere' :: Num a => [a] -> a -> StdGen -> IO a
labouchere' [] acc gen = return acc
labouchere' (x:[]) acc gen = do
  let (randNumber, newGen) = randomR (1,37) gen :: (Int, StdGen)
  if roulette!!randNumber `elem` red
    then return $ acc + x
    else labouchere' (x:[x]) (acc - x) newGen
labouchere' (x:xs) acc gen = do
  let (randNumber, newGen) = randomR (1,37) gen :: (Int, StdGen)
  if roulette!!randNumber `elem` red
    then labouchere' (init xs) (acc + x + last xs) newGen
    else labouchere' ((x:xs) ++ [x + last xs]) (acc - x - last xs) newGen

roulette = [0..37]

red = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]


