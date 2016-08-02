module BreakingRepeatingKeyXor where

import Data.List
import Xor
import HexString

hammingDistance :: String -> String -> Int
hammingDistance s1 s2 =
  let xored = xorStr (encode s1) (encode s2)
  in length (filter id $ hexStrToBitList xored)

