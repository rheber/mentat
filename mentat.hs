import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import Text.Read (readMaybe)

import Getopt (Operation(..), Options(..), defaultOptions, parseArgs)
import Random (digits,
  randIntsTakePairs, randIntsTake, noTensTakePairs, noTensTake)

op2func :: Operation -> (Int -> Int -> Int -> IO Bool)
op2func op = case op of
  Add -> addProblem
  Mul -> mulProblem
  Square -> squareProblem
  Sub -> subProblem

-- Counts how many items in a list are true.
trueCount :: [Bool] -> Int
trueCount = length . filter id

play :: Int -> Bool -> Operation -> Int -> Int -> IO ()
play lo notens op reps hi = do
  startTime <- getClockTime
  results <- if op == TT
             then ttProblems reps notens lo hi
             else problems reps notens op lo hi
  endTime <- getClockTime

  putStr "\nCorrect answers: "
  putStrLn $ (show $ trueCount results) ++ "/" ++ (show reps)
  putStr "Time taken: "
  putStrLn $ timeDiffToString $ diffClockTimes endTime startTime

-- Prepend spaces until string is at least n chars long.
padString :: Int -> String -> String
padString n = until (\x -> length x >= n) (' ':)

-- Creates the text of a problem.
problemText :: Int -> Int -> String -> Int -> String
problemText a b c d =
  "  " ++ (padString d $ show a) ++
  "\n" ++ c ++ " " ++ (padString d $ show b) ++
  "\n==" ++ (replicate d '=')

-- Parse string as int, defaulting to zero.
readInt :: String -> Int
readInt s = case readMaybe s of
  Nothing -> 0
  Just n  -> n

-- Prints an arithmetic problem and examines the user's answer.
arithmeticProblem :: Int -> Int -> Int -> (Int -> Int -> Int) -> String -> IO Bool
arithmeticProblem a b d op opStr = do
  putChar '\n'
  putStrLn $ problemText (max a b) (min a b) opStr d
  answer <- getLine
  let trueAnswer = abs $ a `op` b
  let isCorrect = trueAnswer == readInt answer
  putStrLn $ if isCorrect
             then "Correct!"
             else "Incorrect: " ++ (show trueAnswer)
  return isCorrect

addProblem a b d = arithmeticProblem a b d (+) "+"
subProblem a b d = arithmeticProblem a b d (-) "-"
mulProblem a b d = arithmeticProblem a b d (*) "*"
squareProblem a _ d = arithmeticProblem a a d (*) "*"

-- Asks r problems.
problems :: Int -> Bool -> Operation -> Int -> Int -> IO [Bool]
problems r notens op lo hi = do
  pairs <- (if notens then noTensTakePairs else randIntsTakePairs) r lo hi
  let problem = \(a,b) -> (op2func op) a b $ digits hi
  sequence $ problem <$> pairs

-- Ask r mul problems all involving hi.
ttProblems :: Int -> Bool -> Int -> Int -> IO [Bool]
ttProblems r notens lo hi = do
  rands <- (if notens then noTensTake else randIntsTake) r lo hi
  let problem = \b -> mulProblem hi b $ digits hi
  sequence $ problem <$> rands

main :: IO ()
main = do
  actions <- parseArgs
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optLower = lower,
                optNotens = notens,
                optOp = op,
                optReps = reps,
                optUpper = upper } = opts
  if upper < lower
  then error "Upper bound less than lower bound"
  else if lower < 0
       then error "Negative lower bound"
       else play lower notens op reps upper
