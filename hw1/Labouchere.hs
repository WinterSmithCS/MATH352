-- file Labouchere.hs
-- A program to simulate the Labouchere system

import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

labouchere :: Num a => [a] -> a
labouchere xs = labouchere' xs 0

roulette = [0..37]

red = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,26]

labouchere' :: Num a => [a] -> a -> a
labouchere' [] acc     = acc
labouchere' (x:[]) acc = if unsafePerformIO (pick roulette) `elem` red
                         then acc + x
                         else labouchere' (x:[x]) (acc - x)
labouchere' (x:xs) acc = if unsafePerformIO (pick roulette) `elem` red
                         then labouchere' (init xs) (acc + x + last xs)
                         else labouchere' ((x:xs) ++ [x + last xs]) (acc - x - last xs)
