module Main where

data MMaybe a = MJust a | MNothing

main :: IO ()
main = putStrLn $ show (5 :: Int)