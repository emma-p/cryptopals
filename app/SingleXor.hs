module SingleXor where

import Xor
import Data.List.Split
import Data.List
import Data.Char
import Data.Hex
import qualified Data.String.Utils as StringUtils
import qualified Data.Map.Strict as Map
import Control.Monad.Identity
import Data.Maybe
import HexChar

type Score = Int

instance Show HexChar where
  show = show . toChar

expandKey :: Int -> (HexChar, HexChar) -> HexString
expandKey n (c1, c2) = concat $ replicate (n `div` 2) [c1, c2]

decrypt :: Char -> HexString -> HexString
decrypt key word =
    let keyStr = [key]
        keyHexStr = map toLower $ hex keyStr
        [hex1, hex2] = map fromChar keyHexStr
        keyReplicated = expandKey (length word) (hex1, hex2)
        decrypted = xorStr word keyReplicated
    in decrypted

hexToStr :: HexString -> String
hexToStr str =
    let chunks = chunksOf 2 str
        chunksToChar = map (map toChar) chunks
        decoded = runIdentity $ mapM unhex chunksToChar
    in  concat decoded

scoreChar :: Char -> Float
scoreChar c = Map.findWithDefault (- 10.0) c frequencies

score :: String -> Float
score = sum . map scoreChar

decryptedScores :: HexString -> [(String, Float)]
decryptedScores message =
  let chars = map chr [0..127]
      decrypted = map (hexToStr . flip decrypt message) chars
      scores = map score decrypted
  in sortOn snd $ zip decrypted scores

bestScore :: [(String, Float)] -> (String, Float)
bestScore = foldl (\ (m1, s1) (m2, s2) -> if s1 > s2 then (m1, s1) else (m2, s2)) ("No message found", 0)

bestMessage :: [HexString] -> (String, Float)
bestMessage messages =
  let decrypted = map decryptedScores messages
      bestMessages = map bestScore decrypted
  in bestScore bestMessages

frequencies :: Map.Map Char Float
frequencies =
  Map.fromList [
  ('a', 8.167),
  ('b', 1.492),
  ('c', 2.782),
  ('d', 4.253),
  ('e', 12.702),
  ('f', 2.228),
  ('g', 2.015),
  ('h', 6.094),
  ('i', 6.966),
  ('j', 0.153),
  ('k', 0.772),
  ('l', 4.025),
  ('m', 2.406),
  ('n', 6.749),
  ('o', 7.507),
  ('p', 1.929),
  ('q', 0.095),
  ('r', 5.987),
  ('s', 6.327),
  ('t', 9.056),
  ('u', 2.758),
  ('v', 0.978),
  ('w', 2.361),
  ('x', 0.15),
  ('y', 1.974),
  ('z', 0.074),
  (' ', 5),
  (',', 2),
  ('.', 2)
  ]
