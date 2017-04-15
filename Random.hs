module Random where

import System.Random (RandomGen, StdGen, getStdGen, randomRs)

-- Counts the amout of digits in an integer.
digits :: Int -> Int
digits n = ceiling $ logBase 10 $ 1 + fromIntegral n

-- Checks if x is a power of 10.
isTen :: Int -> Bool
isTen x = let d = digits x in x == 10 ^ (d - 1)

noTens :: [Int] -> [Int]
noTens s = filter (not . isTen) s

-- Produces an infinite list of integers between lo and hi.
randInts :: (RandomGen g) => Int -> Int -> g -> [Int]
randInts lo hi = randomRs (lo, hi)

-- Seeds an integer RNG.
seed :: (StdGen -> [Int]) -> IO [Int]
seed s = getStdGen >>= return . s

-- Produces a list of n integers between lo and hi.
randIntsTake :: Int -> Int -> Int -> IO [Int]
randIntsTake n lo hi = fmap (take n) (seed $ randInts lo hi)

noTensTake :: Int -> Int -> Int -> IO [Int]
noTensTake n lo hi =
  let s g = noTens $ randInts lo hi $ g
  in fmap (take n) $ seed s

-- Produces a list of 2n pairs of integers between lo and hi.
randIntsTakePairs :: Int -> Int -> Int -> IO [(Int, Int)]
randIntsTakePairs n lo hi = do
  aList <- randIntsTake (n * 2) lo hi
  let bList = drop n aList
  return $ zip aList bList

noTensTakePairs :: Int -> Int -> Int -> IO [(Int, Int)]
noTensTakePairs n lo hi = do
  aList <- noTensTake (n * 2) lo hi
  let bList = drop n aList
  return $ zip aList bList
