{-
Main function.
-}

import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random
import System.Time

data Options = Options {
  optDigits :: Int,
  optReps   :: Int
}

defaultOptions :: Options
defaultOptions = Options {
  optDigits = 3,
  optReps   = 5
}

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

options :: [OptDescr (Options -> IO Options)]
options =
  [Option "d" ["digits"]  (ReqArg digAction "D")
       "generate D digit problems, default 3",
   Option "r" ["reps"]    (ReqArg repAction "R")
       "generate R problems, default 5",
   Option "v" ["version"] (NoArg showVersion) "show version number"]

showVersion _ = do
  putStrLn "mentat v0.1"
  exitWith ExitSuccess

digAction :: String -> Options -> IO Options
digAction arg opt = return opt {optDigits = read arg}

repAction :: String -> Options -> IO Options
repAction arg opt = return opt {optReps = read arg}

-- Simply parses the options.
runGetOpt :: [String] -> IO ([Options -> IO Options], [String])
runGetOpt args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
      where header = "Options:"

play :: Int -> Int -> IO ()
play digits reps = do
  startTime <- getClockTime
  results <- sumProblems reps digits
  endTime <- getClockTime

  putStr "Correct answers: "
  putStrLn $ (show $ trueCount results) ++ "/" ++ (show reps)
  putStr "Time taken: "
  putStrLn $ timeDiffToString $ diffClockTimes endTime startTime

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
  args <- getArgs
  (actions, _) <- runGetOpt args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optDigits = digits, optReps = reps } = opts
  play digits reps
