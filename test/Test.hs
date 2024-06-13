module Main where

import Data.List (sort)
import Peeksort
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

modelSpec :: Spec
modelSpec = describe "peeksort" $ do
  modifyMaxSuccess (const 1000) $ it "is same as model" $ property prop_model

prop_model :: [Int] -> Property
prop_model xs = sort xs === peeksort xs

main :: IO ()
main = hspec modelSpec 
