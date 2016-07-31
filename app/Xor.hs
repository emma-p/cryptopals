module Xor where

import Data.List.Split
import HexChar
import HexString

type BitList = [Bool]

hexToBitList :: HexChar -> BitList
hexToBitList Hex0 = [False, False, False, False]
hexToBitList Hex1 = [False, False, False, True]
hexToBitList Hex2 = [False, False, True, False]
hexToBitList Hex3 = [False, False, True, True]
hexToBitList Hex4 = [False, True, False, False]
hexToBitList Hex5 = [False, True, False, True]
hexToBitList Hex6 = [False, True, True, False]
hexToBitList Hex7 = [False, True, True, True]
hexToBitList Hex8 = [True, False, False, False]
hexToBitList Hex9 = [True, False, False, True]
hexToBitList HexA = [True, False, True, False]
hexToBitList HexB = [True, False, True, True]
hexToBitList HexC = [True, True, False, False]
hexToBitList HexD = [True, True, False, True]
hexToBitList HexE = [True, True, True, False]
hexToBitList HexF = [True, True, True, True]
hexToBitList _ = error "Invalid character"

bitListToHex :: BitList -> HexChar
bitListToHex [False, False, False, False]= Hex0
bitListToHex [False, False, False, True] = Hex1
bitListToHex [False, False, True, False] = Hex2
bitListToHex [False, False, True, True]  = Hex3
bitListToHex [False, True, False, False] = Hex4
bitListToHex [False, True, False, True]  = Hex5
bitListToHex [False, True, True, False]  = Hex6
bitListToHex [False, True, True, True]   = Hex7
bitListToHex [True, False, False, False] = Hex8
bitListToHex [True, False, False, True]  = Hex9
bitListToHex [True, False, True, False]  = HexA
bitListToHex [True, False, True, True]   = HexB
bitListToHex [True, True, False, False]  = HexC
bitListToHex [True, True, False, True]   = HexD
bitListToHex [True, True, True, False]   = HexE
bitListToHex [True, True, True, True]    = HexF
bitListToHex _ = error "Invalid BitList"

hexStrToBitList :: HexString -> BitList
hexStrToBitList = (>>= hexToBitList)

xor :: Bool -> Bool -> Bool
xor True True = False
xor a b = a || b

xorStr :: HexString -> HexString -> HexString
xorStr a b =
    let bitList = zipWith xor (hexStrToBitList a) (hexStrToBitList b)
    in map bitListToHex $ chunksOf 4 bitList
