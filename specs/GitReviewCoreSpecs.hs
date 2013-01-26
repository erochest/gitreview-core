module Main where

import Github.Review.FiltersSpec (filtersSpec)
import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

main :: IO ()
main = hspec filtersSpec

