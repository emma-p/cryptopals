module RepeatingKeyXor where

import HexChar
import Data.Hex
import Data.Char
import Xor

message :: String
message = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"

expandRepeatingKey :: Int -> HexString -> HexString
expandRepeatingKey n hexStr = concat $ replicate (n `div` 2) hexStr

-- Xor hex string with repeating key of same length
encryptWithRepeatingKey :: String ->  String
encryptWithRepeatingKey key  =
    let hexKey = strToHexString key
        keyReplicated = expandRepeatingKey (length message) hexKey
        encrypted = xorStr (strToHexString message) keyReplicated
    in (map toChar encrypted)
