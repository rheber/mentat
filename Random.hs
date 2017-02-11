module Random where

import System.Random (getStdGen, randomRs)

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

-- Produces a list of 2n pairs of integers which each conatin hi.
randIntsTTPairs :: Int -> Int -> Int -> IO [(Int, Int)]
randIntsTTPairs n lo hi = do
  rands <- randIntsTake n lo hi
  return $ zip (repeat hi) rands
