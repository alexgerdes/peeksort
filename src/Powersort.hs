{-# LANGUAGE BangPatterns #-}

module Powersort ( powersort ) where

import           Data.Bifunctor
import           Data.List       ( sort )
import           Test.QuickCheck

-- Runs -----------------------------------------------------------------------
type Power = Int

type Run a = ([a], Power)

powerRuns :: Ord a => [a] -> [Run a]
powerRuns = powerize . go 0
 where
  go i []       = (i, [])
  go i [x]      = (i, [((i, i + 1), [x])])
  go i (x : y : ys)
    | x > y     = dsc i (i + 2) y [x] ys
    | otherwise = asc i (i + 2) y (x :) ys

  dsc i j x xs (y : ys)
    | x > y       = dsc i (j + 1) y (x : xs) ys
  dsc i j x xs ys = second (((i, j - 1), x : xs) :) (go j ys)

  asc i j x xs (y : ys)
    | x <= y      = asc i (j + 1) y (\zs -> xs (x : zs)) ys
  asc i j x xs ys = let !zs = xs [x] in second (((i, j - 1), zs) :) (go j ys)

  powerize (n, rs) = go rs
   where
    go rs = case rs of
        (p, xs) : (q, ys) : rs -> (xs, power n p q) : go ((q, ys) : rs)
        [(_, xs)] -> [(xs, 0)]
        _ -> []

  power n (i1, j1) (i2, j2) = go 0 1
   where
    d1 = i1 + (j1 - i1) `div` 2 - 1
    d2 = i2 + (j2 - i2) `div` 2 - 1

    go l m
      | (m * d1) `div` n /= (m * d2) `div` n = l
      | otherwise = go (l + 1) (2 * m)

-- Merge runs -----------------------------------------------------------------
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys   = ys
merge xs []   = xs
merge (x : xs) (y : ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- Powersort ------------------------------------------------------------------
powersort :: Ord a => [a] -> [a]
powersort = go [] . powerRuns
 where
  go stack (x@(_, px) : y@(_, py) : rs)
    | px <= py  = go (x : stack) (y : rs)
    | otherwise = go (fix (x : stack)) (y : rs)
  go stack [r]  = go (r : stack) []
  go stack []   = foldl (\acc (xs, _) -> merge xs acc) [] stack

  fix ((xs, px) : (ys, py) : rs)
    | px <= py = fix ((merge ys xs, px) : rs)
  fix rs       = rs

-- Test stuff -----------------------------------------------------------------
prop_model :: [Int] -> Property
prop_model xs = sort xs === powersort xs

inp0, inp1, inp2, inp3, inp4 :: [Int]
inp0 =
    [ 27
    , 28
    , 29
    , 30
    , 31
    , 32
    , 33
    , 34
    , 35
    , 36
    , 37
    , 38
    , 39
    , 40
    , 41
    , 42
    , 43
    , 44
    , 45
    , 46
    , 47
    , 48
    , 49
    , 50
    , 51
    , 52
    , 53
    , 54
    , 55
    , 56
    , 57
    , 58
    , 59
    , 60
    , 61
    , 62
    , 63
    , 64
    , 24
    , 25
    , 26
    , 21
    , 22
    , 23
    , 18
    , 19
    , 20
    , 15
    , 16
    , 17
    , 12
    , 13
    , 14
    , 9
    , 10
    , 11
    , 6
    , 7
    , 8
    , 3
    , 4
    , 5
    , 1
    , 2
    ]

inp1 =
    [ 63
    , 64
    , 62
    , 61
    , 58
    , 59
    , 60
    , 57
    , 54
    , 55
    , 56
    , 52
    , 53
    , 50
    , 51
    , 49
    , 46
    , 47
    , 48
    , 44
    , 45
    , 42
    , 43
    , 39
    , 40
    , 41
    , 37
    , 38
    , 36
    , 34
    , 35
    , 33
    , 30
    , 31
    , 32
    , 28
    , 29
    , 26
    , 27
    , 23
    , 24
    , 25
    , 21
    , 22
    , 20
    , 18
    , 19
    , 15
    , 16
    , 17
    , 13
    , 14
    , 11
    , 12
    , 8
    , 9
    , 10
    , 6
    , 7
    , 5
    , 4
    , 2
    , 3
    , 1
    ]

inp2 = concat [[1 .. 5], [1, 2, 3], [1, 2, 3], [1 .. 14], [1, 2], [1]]

inp3 =
    concat [[1 .. 5], [1, 2, 3], [1, 2, 3], [1 .. 4], [1 .. 10], [1, 2], [1]]

inp4 =
    [ 44
    , 45
    , 46
    , 47
    , 48
    , 49
    , 50
    , 41
    , 42
    , 43
    , 40
    , 39
    , 33
    , 34
    , 35
    , 36
    , 37
    , 38
    , 17
    , 18
    , 19
    , 20
    , 21
    , 22
    , 23
    , 24
    , 25
    , 26
    , 27
    , 28
    , 29
    , 30
    , 31
    , 32
    , 11
    , 12
    , 13
    , 14
    , 15
    , 16
    , 10
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    ]

