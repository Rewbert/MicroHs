module View(main) where
import Prelude

f :: Int -> Int
f ((> (10::Int)) -> True) = 1
f _ = 0

main :: IO ()
main = do
  print (f 20)
  print (f 10)
  