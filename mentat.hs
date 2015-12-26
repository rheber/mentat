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

data Operation = Add | Mul | Sub

str2op :: String -> Operation
str2op t
  | s == "mul" = Mul
  | s == "sub" = Sub
  | otherwise  = Add
  where s = map toLower t

op2func :: Operation -> (Int -> Int -> Int -> IO Bool)
op2func op = case op of
  Add -> addProblem
  Mul -> mulProblem
  Sub -> subProblem

data Options = Options {
  optDigits :: Int,
  optOp     :: Operation,
  optReps   :: Int
}

defaultOptions :: Options
defaultOptions = Options {
  optDigits = 3,
  optOp     = Add,
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
   Option "o" ["operation"] (ReqArg opAction "OP")
       "the kind of problems to generate, one of: add (default), sub, mul",
   Option "r" ["reps"]    (ReqArg repAction "R")
       "generate R problems, default 5",
   Option "v" ["version"] (NoArg showVersion) "show version number"]

showVersion _ = do
  putStrLn "mentat v0.1"
  exitWith ExitSuccess

digAction :: String -> Options -> IO Options
digAction arg opt = return opt {optDigits = read arg}

opAction :: String -> Options -> IO Options
opAction arg opt = return opt {optOp = str2op arg}

repAction :: String -> Options -> IO Options
repAction arg opt = return opt {optReps = read arg}

-- Simply parses the options.
runGetOpt :: [String] -> IO ([Options -> IO Options], [String])
runGetOpt args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
      where header = "Options:"

play :: Int -> Operation -> Int -> IO ()
play digits op reps = do
  startTime <- getClockTime
  results <- problems reps op digits
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

-- Prints an add problem and examines an answer.
addProblem :: Int -> Int -> Int -> IO Bool
addProblem a b d = do
  putChar '\n'
  putStrLn $ problemText a b "+" d
  answer <- getLine
  return (a + b == read answer)

subProblem :: Int -> Int -> Int -> IO Bool
subProblem a b d = do
  putChar '\n'
  putStrLn $ problemText (max a b) (min a b) "-" d
  answer <- getLine
  return ((abs $ a - b) == read answer)

mulProblem :: Int -> Int -> Int -> IO Bool
mulProblem a b d = do
  putChar '\n'
  putStrLn $ problemText a b "x" d
  answer <- getLine
  return (a * b == read answer)

-- Asks r problems.
problems :: Int -> Operation -> Int -> IO [Bool]
problems r op d = do
  pairs <- randIntsTakePairs r d
  let sp (a,b) = (op2func op) a b d
  sequence $ map sp pairs

main :: IO ()
main = do
  args <- getArgs
  (actions, _) <- runGetOpt args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optDigits = digits, optOp = op, optReps = reps } = opts
  play digits op reps
