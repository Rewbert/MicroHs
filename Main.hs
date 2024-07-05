module Main where

data MMaybe a = MJust a | MNothing

-- y :: Int
-- y = 5

-- x :: Maybe Bool
-- x = Just True

-- test :: Num a => a -> Maybe a
-- test x = Just (x + 1)

-- f :: Int
-- f = case test 7 of
--     Just y ->  7
--     Nothing -> 710
main :: IO ()
main = putStrLn $ show (5 :: Int)
