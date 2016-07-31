module HexString where

import HexChar
import Data.Hex
import Data.Char
import Data.List.Split
import Control.Monad.Identity

type HexString = [HexChar]

encode :: String -> HexString
encode str =
  let encoded = hex str
  in map fromChar $ (map toLower encoded)

decode :: HexString -> String
decode str =
    let chunks = chunksOf 2 str
        chunksToChar = map (map toChar) chunks
        decoded = runIdentity $ mapM unhex chunksToChar
    in  concat decoded

-- Create HexString of length n from two repeating hex chars
expandKey :: Int -> (HexChar, HexChar) -> HexString
expandKey n (c1, c2) = concat $ replicate (n `div` 2) [c1, c2]
