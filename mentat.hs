{-
Main function.
-}

import System.Console.GetOpt
import System.Environment
import System.Random
import System.Time

data Flag =
  Version
  deriving Show

-- Produces an infinite list of integers with d digits.
randInts :: Int -> IO [Int]
randInts d = getStdGen >>= return . randomRs (10^(d-1), (10^d)-1)

-- Produces a list of n integers with d digits.
randIntsTake :: Int -> Int -> IO [Int]
randIntsTake n d = fmap (take n) (randInts d)

-- Produces a list of pairs of integers.
randIntsTakePairs :: Int -> Int -> IO [(Int, Int)]
randIntsTakePairs n d = do
  aList <- randIntsTake (n * 2) d
  let bList = drop n aList
  return $ zip aList bList

-- Counts how many items in a list are true.
trueCount :: [Bool] -> Int
trueCount = length . filter id

options :: [OptDescr Flag]
options =
  [Option "v" ["version"] (NoArg Version) "show version number"]

runGetOpt :: [String] -> IO ([Flag], [String])
runGetOpt args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
      where header = "Options:"

flagCase :: Flag -> IO ()
flagCase f =
  case f of
    Version -> print "mentat v0.1"

-- Creates the text of a sum problem.
sumProblemText :: Int -> Int -> Int -> String
sumProblemText a b d = "  " ++ (show a) ++
                     "\n+ " ++ (show b) ++
                     "\n==" ++ (replicate d '=')

-- Prints a sum problem and examines an answer.
sumProblem :: Int -> Int -> Int -> IO Bool
sumProblem a b d = do
  putChar '\n'
  putStrLn $ sumProblemText a b d
  answer <- getLine
  return (a + b == read answer)

-- Asks r sum problems.
sumProblems :: Int -> Int -> IO [Bool]
sumProblems r d = do
  pairs <- randIntsTakePairs r d
  let sp (a,b) = sumProblem a b d
  sequence $ map sp pairs

main :: IO ()
main = do
  let digits = 3
  let reps = 5

  args <- getArgs
  (flags, value) <- runGetOpt args
  sequence $ map flagCase flags

  startTime <- getClockTime
  results <- sumProblems reps digits
  endTime <- getClockTime

  putStr "Correct answers: "
  putStrLn $ (show $ trueCount results) ++ "/" ++ (show reps)
  putStr "Time taken: "
  putStrLn $ timeDiffToString $ diffClockTimes endTime startTime