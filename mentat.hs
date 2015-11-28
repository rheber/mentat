{-
Main function.
-}

import System.Random

-- Produces a random integer with d digits.
randDigits :: Int -> IO Int
randDigits d = getStdRandom $ randomR (10^(d-1), (10^d)-1)

-- Produces an infinite list of integers with d digits.
randIntList :: Int -> IO [Int]
randIntList d = getStdGen >>= return . randomRs (10^(d-1), (10^d)-1)

-- Produces a list of n integers with d digits.
someRandInts :: Int -> Int -> IO [Int]
someRandInts n d = fmap (take n) (randIntList d)

main :: IO ()
main = do
  numList <- someRandInts 10 3
  print numList