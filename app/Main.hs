module Main where

import MergeSort
import Peeksort

import Criterion.Main
import Data.List (unfoldr)
import Test.QuickCheck
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["prof"] -> do
      xs <- read <$> readFile "input.txt"  -- generate (vector (read n) :: Gen [Int]
      print $ last $ sort (xs :: [Int])
    _ -> let n = 10000 in defaultMain
      [ env (setupEnv n) $ \ ~(rnd, srt, rev, nrl, bad, swp) -> bgroup "main" 
        [ bgroup "sort" 
          [ bench "random"  $ nf sort rnd
          , bench "nearly"  $ nf sort nrl
          , bench "bad"     $ nf sort bad
          , bench "sorted"  $ nf sort srt
          , bench "revsort" $ nf sort rev
          , bench "swapped" $ nf sort swp
          ]
        , bgroup "peeksort" 
          [ bench "random"  $ nf peeksort rnd
          , bench "nearly"  $ nf peeksort nrl
          , bench "bad"     $ nf peeksort bad
          , bench "sorted"  $ nf peeksort srt
          , bench "revsort" $ nf peeksort rev
          , bench "swapped" $ nf peeksort swp
          ]
        ]
      ]

setupEnv :: Int -> IO ([Int], [Int], [Int], [Int], [Int], [Int])
setupEnv n = do
  rnd <- generate (vector n)
  nrl <- concat <$> generate (gRuns n)
  let srt = [1 .. n]
      rev = reverse srt
      bad = maf srt
  swp <- generate $ swapGen (n `div` 100) srt
  return (rnd, srt, rev, nrl, bad, swp)

swapGen :: Int -> [a] -> Gen [a]
swapGen 0 xs = return xs
swapGen n xs = do 
  (i, j) <- gPos
  swapGen (n-1) (swap i j xs) 
 where
  l = length xs
  gPos = (,) <$> choose (0, l-1) <*> choose (0, l-1)

swap :: Int -> Int -> [a] -> [a]
swap i j xs 
  | i == j    = xs
  | otherwise = take n xs ++ y ++ drop 1 (take m bs) ++ x ++ drop 1 ds
 where
  (n, m) = (min i j, max i j - n)
  bs = drop n xs
  x  = take 1 bs
  ds = drop m bs
  y  = take 1 ds

prop_swapIndex :: Int -> Int -> [Int] -> Property
prop_swapIndex i j xs = swap i j xs === swap j i xs

prop_swapLength :: Int -> Int -> [Int] -> Property
prop_swapLength i j xs = length xs === length (swap i j xs)

maf []  = []
maf [x] = [x]
maf xs  = let (as, bs) = splitAt (length xs `div` 2) xs
              (cs, ds) = splitAt (length bs `div` 2) bs 
          in  as ++ reverse cs ++ maf ds

gRuns :: Int -> Gen [[Int]]
gRuns n 
  | n > 1 = do
      xs <- infiniteListOf $ frequency [ (20, choose (1, 6))
                                       , (10, choose (7, 20))
                                       , (1, choose (1, n `div` 3))]
      mapM gRun $ f n xs
  | otherwise = return []

f n xs = let ys = unfoldr g (0, xs) in (n - sum ys) : ys
 where
  g (m, (x:xs)) 
    | m + x < n = Just (x, (m + x, xs)) 
    | otherwise = Nothing

gRun :: (Arbitrary a, Ord a) => Int -> Gen [a]
gRun n = let v = fmap sort (vector n) in oneof [v, fmap reverse v] 
  
