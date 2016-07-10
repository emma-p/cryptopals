module Xor where

import Data.List.Split

type BitList = [Bool]

hexToBitList :: Char -> BitList
hexToBitList '0' = [False, False, False, False]
hexToBitList '1' = [False, False, False, True]
hexToBitList '2' = [False, False, True, False]
hexToBitList '3' = [False, False, True, True]
hexToBitList '4' = [False, True, False, False]
hexToBitList '5' = [False, True, False, True]
hexToBitList '6' = [False, True, True, False]
hexToBitList '7' = [False, True, True, True]
hexToBitList '8' = [True, False, False, False]
hexToBitList '9' = [True, False, False, True]
hexToBitList 'a' = [True, False, True, False]
hexToBitList 'b' = [True, False, True, True]
hexToBitList 'c' = [True, True, False, False]
hexToBitList 'd' = [True, True, False, True]
hexToBitList 'e' = [True, True, True, False]
hexToBitList 'f' = [True, True, True, True]
hexToBitList _ = error "Invalid character"

bitListToHex :: BitList -> Char
bitListToHex [False, False, False, False]= '0'
bitListToHex [False, False, False, True] = '1'
bitListToHex [False, False, True, False] = '2'
bitListToHex [False, False, True, True]  = '3'
bitListToHex [False, True, False, False] = '4'
bitListToHex [False, True, False, True]  = '5'
bitListToHex [False, True, True, False]  = '6'
bitListToHex [False, True, True, True]   = '7'
bitListToHex [True, False, False, False] = '8'
bitListToHex [True, False, False, True]  = '9'
bitListToHex [True, False, True, False]  = 'a'
bitListToHex [True, False, True, True]   = 'b'
bitListToHex [True, True, False, False]  = 'c'
bitListToHex [True, True, False, True]   = 'd'
bitListToHex [True, True, True, False]   = 'e'
bitListToHex [True, True, True, True]    = 'f'
bitListToHex _ = error "Invalid BitList"

hexStrToBitList :: String -> BitList
hexStrToBitList = (>>= hexToBitList)

xor :: Bool -> Bool -> Bool
xor True True = False
xor a b = a || b

xorStr :: String -> String -> String
xorStr a b =
    let bitList = zipWith xor (hexStrToBitList a) (hexStrToBitList b)
    in map bitListToHex $ chunksOf 4 bitList
