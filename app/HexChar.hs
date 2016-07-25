module HexChar where

import Data.Hex
import Data.Char

data HexChar
  = Hex0
  | Hex1
  | Hex2
  | Hex3
  | Hex4
  | Hex5
  | Hex6
  | Hex7
  | Hex8
  | Hex9
  | HexA
  | HexB
  | HexC
  | HexD
  | HexE
  | HexF
  deriving (Eq, Enum, Bounded)

toChar :: HexChar -> Char
toChar Hex0 = '0'
toChar Hex1 = '1'
toChar Hex2 = '2'
toChar Hex3 = '3'
toChar Hex4 = '4'
toChar Hex5 = '5'
toChar Hex6 = '6'
toChar Hex7 = '7'
toChar Hex8 = '8'
toChar Hex9 = '9'
toChar HexA = 'a'
toChar HexB = 'b'
toChar HexC = 'c'
toChar HexD = 'd'
toChar HexE = 'e'
toChar HexF = 'f'

charHexChar :: [(Char, HexChar)]
charHexChar = map (\hex -> (toChar hex, hex)) [minBound..maxBound]

fromChar :: Char -> HexChar
fromChar c =
  case lookup c charHexChar of
    Just h -> h
    _ -> error $ "Error: unexpected character: " ++ show c

type HexString = [HexChar]

strToHexString :: String -> HexString
strToHexString str =
  let encoded = hex str
  in map fromChar $ (map toLower encoded)
