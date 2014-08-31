-- file: Martingale.hs
-- a program to simulate the martingale doubling system

import System.Random (randomRIO, StdGen)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

roulette = [0..37]

red = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]

martingale :: Real a => a -> a -> Bool
martingale acc bet
  | acc >= 5     = True
  | acc <= -100  = False
  | otherwise    = if unsafePerformIO (pick roulette) `elem` red
                   then martingale (acc + bet) 1
                   else martingale (acc - bet) (bet * 2)

main :: IO ()
main = do
  args <- getArgs
  let iters = read $ head args
  let countWinnings = f 0 0 where
        f acc wins
          | acc < iters = f (acc + 1) (if martingale 0 1 then wins + 1 else wins)
          | otherwise   = wins
  print $ (fromIntegral countWinnings) / (fromIntegral iters)


