module Main (main) where

import Config (prop_configuration_allowed)
import Test.QuickCheck (maxSuccess, quickCheckWith, stdArgs)

main :: IO ()
main = quickCheckWith stdArgs {maxSuccess = 10} prop_configuration_allowed
