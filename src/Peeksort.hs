{-# LANGUAGE BangPatterns #-}

module Peeksort
  ( peeksort
  ) where

import Data.Bifunctor
import Data.List (sort)
import Test.QuickCheck

-- Runs -----------------------------------------------------------------------
type Run a = (Int, [a])

runs :: Ord a => [a] -> (Int, [Run a])
runs []         = (0, [])
runs [x]        = (1, [(1, [x])])
runs (x:y:ys)
  | x > y       = dsc 2 y [x]  ys
  | otherwise   = asc 2 y (x:) ys 
 where
  dsc n a as (b:bs)
    | a > b     = dsc (n+1) b (a:as) bs
  dsc n a as bs = let !m = n in bimap (m+) ((m, a:as):) (runs bs)

  asc n a as (b:bs)
    | a <= b    = asc (n+1) b (\zs -> as (a:zs)) bs
  asc n a as bs = let !zs = as [a]; !m = n in bimap (m+) ((m, zs):) (runs bs)

-- Merge runs -----------------------------------------------------------------
mergeRuns :: Ord a => (Int, [Run a]) -> [a]
mergeRuns (_, [])  = []
mergeRuns (_, [x]) = snd x
mergeRuns (n, rs)  = go (0, id) rs
 where
  mid = n `div` 2
  
  go (a, l) []  = mergeRuns (a, l [])
  go (a, l) xs'@(x@(m, _):xs)
    | snap      = go (a + m, l . (x:)) xs 
    | otherwise = mergeRuns (a, l []) `merge` mergeRuns (n - a, xs')
    where
     snap = a == 0 || m `div` 2 + a <= mid 

  merge l@(x:xs) r@(y:ys) 
    | x <= y    = x : merge xs r
    | otherwise = y : merge l ys
  merge [] ys   = ys
  merge xs []   = xs

-- Peeksort -------------------------------------------------------------------
peeksort :: Ord a => [a] -> [a]
peeksort xs = mergeRuns (runs xs)

-- Test stuff -----------------------------------------------------------------
prop_model :: [Int] -> Property
prop_model xs = sort xs === peeksort xs

inp0, inp1, inp2, inp3 :: [Int]
inp0 = [27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 24, 25, 26, 21, 22, 23, 18, 19, 20, 15, 16, 17, 12, 13, 14, 9, 10, 11, 6, 7, 8, 3, 4, 5, 1, 2]
inp1 = [63, 64, 62, 61, 58, 59, 60, 57, 54, 55, 56, 52, 53, 50, 51, 49, 46, 47, 48, 44, 45, 42, 43, 39, 40, 41, 37, 38, 36, 34, 35, 33, 30, 31, 32, 28, 29, 26, 27, 23, 24, 25, 21, 22, 20, 18, 19, 15, 16, 17, 13, 14, 11, 12, 8, 9, 10, 6, 7, 5, 4, 2, 3, 1]
inp2 = concat [[1..5],[1,2,3],[1,2,3],[1..14],[1,2],[1]]
inp3 = concat [[1..5],[1,2,3],[1,2,3],[1..4],[1..10],[1,2],[1]]

-- State version --------------------------------------------------------------
spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ []     = return ([], [])
spanM p (x:xs) = ifM (p x) (first (x:) <$> spanM p xs) (return ([], x:xs))

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM m x y = m >>= \b -> if b then x else y

-- mergeRs :: Ord a => (Int, [Run a]) -> [a]
-- mergeRs (_, [])  = []
-- mergeRs (_, [x]) = snd x
-- mergeRs (n, rs)  = mergeRs (m, xs) `merge` mergeRs (n - m, ys) 
--  where
--   mid = n `div` 2
--
--   ((xs, ys), m) = runState (spanM snapTo rs) 0
--
--   snapTo :: Run a -> State Int Bool
--   snapTo (n, _) = get >>= \acc -> 
--     let b = acc == 0 || n `div` 2 + acc <= mid 
--     in  when b (modify (+ n)) >> return b

