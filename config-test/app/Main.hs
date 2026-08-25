module Main (main) where

import Config (prop_configuration_allowed, resetTmp)
import Test.QuickCheck (maxSuccess, quickCheckWith, stdArgs)

main :: IO ()
main = do
  resetTmp
  quickCheckWith stdArgs {maxSuccess = 10} prop_configuration_allowed
