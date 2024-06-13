{-# LANGUAGE BangPatterns #-}

module Timsort
  ( timsort
  ) where
  
type Run a = (Int, [a])

runs :: Ord a => [a] -> [Run a]
runs (x:y:ys)
  | x > y      = dscRun 2 y [x]  ys
  | otherwise  = ascRun 2 y (x:) ys 
runs [x]       = [(1, [x])]
runs _         = []

dscRun :: Ord a => Int -> a -> [a] -> [a] -> [Run a]
dscRun n x xs (y:ys)
  | x > y        = dscRun (n+1) y (x:xs) ys
dscRun n x xs ys = (n, x:xs) : runs ys

ascRun :: Ord a => Int -> a -> ([a] -> [a]) -> [a] -> [Run a]
ascRun n x xs (y:ys)
  | x <= y       = ascRun (n+1) y (\zs -> xs (x:zs)) ys
ascRun n x xs ys = let !zs = xs [x] in (n, zs) : runs ys

fix :: Ord a => [Run a] -> [Run a]
fix ((xn, xs) : (yn, ys) : (zn, zs) : zss)
  | zn <= yn + xn || yn <= xn = fix stack'
    where
      stack' | xn < zn   = (xn + yn, merge xs ys) : (zn, zs) : zss
             | otherwise = (xn, xs) : (yn + zn, merge ys zs) : zss
fix ((xn, xs) : (yn, ys) : yss)
  | xn >= yn = (xn + yn, merge xs ys) : yss
fix stack = stack

timsort :: Ord a => [a] -> [a]
timsort = mergeAll . map snd . foldr (\x xs -> fix (x:xs)) [] . runs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll list = case list of
  []   -> []
  [xs] -> xs
  xss  -> mergeAll (mergePairs xss)
 where
  mergePairs (xs:ys:yss) = merge xs ys : mergePairs yss
  mergePairs xss         = xss
