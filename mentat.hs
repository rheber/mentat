{-
Main function.
-}

import Data.Char
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random
import System.Time
import Text.Read

data Operation = Add | Mul | Square | Sub

str2op :: String -> Operation
str2op t
  | s == "mul" = Mul
  | s == "square" = Square
  | s == "sub" = Sub
  | otherwise  = Add
  where s = map toLower t

op2func :: Operation -> (Int -> Int -> Int -> IO Bool)
op2func op = case op of
  Add -> addProblem
  Mul -> mulProblem
  Square -> squareProblem
  Sub -> subProblem

data Options = Options {
  optLower :: Int,
  optOp    :: Operation,
  optReps  :: Int,
  optUpper :: Int
}

defaultOptions :: Options
defaultOptions = Options {
  optLower = 100,
  optOp    = Add,
  optReps  = 5,
  optUpper = 999
}

-- Counts the amout of digits in an integer.
digits :: Int -> Int
digits n = ceiling $ logBase 10 $ 1 + fromIntegral n

-- Produces an infinite list of integers between lo and hi.
randInts :: Int -> Int -> IO [Int]
randInts lo hi = getStdGen >>= return . randomRs (lo, hi)

-- Produces a list of n integers between lo and hi.
randIntsTake :: Int -> Int -> Int -> IO [Int]
randIntsTake n lo hi = fmap (take n) (randInts lo hi)

-- Produces a list of 2n pairs of integers between lo and hi.
randIntsTakePairs :: Int -> Int -> Int -> IO [(Int, Int)]
randIntsTakePairs n lo hi = do
  aList <- randIntsTake (n * 2) lo hi
  let bList = drop n aList
  return $ zip aList bList

-- Counts how many items in a list are true.
trueCount :: [Bool] -> Int
trueCount = length . filter id

options :: [OptDescr (Options -> IO Options)]
options =
  [Option "l" ["lower"]  (ReqArg lowerAction "LO")
       "minimum value of each operand, default 100",
   Option "o" ["operation"] (ReqArg opAction "OP")
       "the kind of problems to generate, one of: add (default), sub, mul",
   Option "r" ["reps"]    (ReqArg repAction "R")
       "generate R problems, default 5",
   Option "u" ["upper"]  (ReqArg upperAction "HI")
       "maximum value of each operand, default 999",
   Option "v" ["version"] (NoArg showVersion) "show version number"]

showVersion _ = do
  putStrLn "mentat v0.1"
  exitWith ExitSuccess

lowerAction :: String -> Options -> IO Options
lowerAction arg opt = return opt {optLower = read arg}

opAction :: String -> Options -> IO Options
opAction arg opt = return opt {optOp = str2op arg}

repAction :: String -> Options -> IO Options
repAction arg opt = return opt {optReps = read arg}

upperAction :: String -> Options -> IO Options
upperAction arg opt = return opt {optUpper = read arg}

-- Simply parses the options.
runGetOpt :: [String] -> IO ([Options -> IO Options], [String])
runGetOpt args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
      where header = "Options:"

play :: Int -> Operation -> Int -> Int -> IO ()
play lo op reps hi = do
  startTime <- getClockTime
  results <- problems reps op lo hi
  endTime <- getClockTime

  putStr "Correct answers: "
  putStrLn $ (show $ trueCount results) ++ "/" ++ (show reps)
  putStr "Time taken: "
  putStrLn $ timeDiffToString $ diffClockTimes endTime startTime

-- Creates the text of a problem.
problemText :: Int -> Int -> String -> Int -> String
problemText a b c d = "  " ++ (show a) ++
          "\n" ++ c ++ " " ++ (show b) ++
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
  return ((abs $ a `op` b) == readInt answer)

addProblem a b d = arithmeticProblem a b d (+) "+"
subProblem a b d = arithmeticProblem a b d (-) "-"
mulProblem a b d = arithmeticProblem a b d (*) "*"
squareProblem a _ d = arithmeticProblem a a d (*) "*"

-- Asks r problems.
problems :: Int -> Operation -> Int -> Int -> IO [Bool]
problems r op lo hi = do
  pairs <- randIntsTakePairs r lo hi
  let sp (a,b) = (op2func op) a b $ digits hi
  sequence $ map sp pairs

main :: IO ()
main = do
  args <- getArgs
  (actions, _) <- runGetOpt args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optLower = lower,
                optOp = op,
                optReps = reps,
                optUpper = upper } = opts
  play lower op reps upper
