{-
Main function.
-}

import System.Random
import System.Time

-- Produces an infinite list of integers with d digits.
randIntList :: Int -> IO [Int]
randIntList d = getStdGen >>= return . randomRs (10^(d-1), (10^d)-1)

-- Produces a list of n integers with d digits.
someRandInts :: Int -> Int -> IO [Int]
someRandInts n d = fmap (take n) (randIntList d)

-- Creates the text of a sum problem.
sumProblemText :: Int -> Int -> Int -> String
sumProblemText a b d = "  " ++ (show a) ++
                     "\n+ " ++ (show b) ++
                     "\n==" ++ (replicate d '=')

main :: IO ()
main = do
  let digits = 3
  numList <- someRandInts 2 digits

  startTime <- getClockTime
  putStrLn $ sumProblemText (head numList) (numList !! 1) digits
  endTime <- getClockTime

  putStr "Time taken: "
  putStrLn $ timeDiffToString $ diffClockTimes endTime startTime