module ListTest where
import Prelude
import List
import IO

main = IO.do
  print (sum [1,2,3])
  print sum
  print (and [True])
  print and
  print (&&)
  