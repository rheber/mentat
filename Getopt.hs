module Getopt where

import Data.Char (toLower)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess), exitWith)

data Operation = Add | Mul | Square | Sub | TT
  deriving Eq

data Options = Options {
  optLower :: Int,
  optOp    :: Operation,
  optReps  :: Int,
  optUpper :: Int
}

defaultOptions :: Options
defaultOptions = Options {
  optLower = 1,
  optOp    = Add,
  optReps  = 5,
  optUpper = 999
}

str2op :: String -> Operation
str2op t
  | s == "mul" = Mul
  | s == "square" = Square
  | s == "sub" = Sub
  | s == "tt" = TT
  | otherwise  = Add
  where s = map toLower t

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn "mentat v0.1"
  exitWith ExitSuccess

options :: [OptDescr (Options -> IO Options)]
options =
  [Option "l" ["lower"]  (ReqArg lowerAction "LO")
       "minimum value of each operand, default 1",
   Option "o" ["operation"] (ReqArg opAction "OP")
       "the kind of problems to generate, one of: add (default), sub, mul, square, tt",
   Option "r" ["reps"]    (ReqArg repAction "R")
       "generate R problems, default 5",
   Option "u" ["upper"]  (ReqArg upperAction "HI")
       "maximum value of each operand, default 999",
   Option "v" ["version"] (NoArg showVersion) "show version number"]

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

-- Get and parse args.
parseArgs :: IO [Options -> IO Options]
parseArgs = do
  args <- getArgs
  (actions, _) <- runGetOpt args
  return actions
