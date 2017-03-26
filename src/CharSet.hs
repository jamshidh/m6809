
module CharSet (
  word8ToChar
  ) where

import qualified Data.Vector as V
import Data.Word
import System.Console.ANSI

regular::V.Vector Char
regular=V.fromList "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]↑← !\"#$%&'()*+,-./0123456789:;<=>?"

block::V.Vector Char
--block=V.fromList " ▗▖▄▝▐▞▟▘▚▌▙▀▜▛█"
block=V.fromList "█▛▜▀▙▌▚▘▟▞▐▝▄▖▗ "

--A "char" can have control codes (for colors), so I return a String
word8ToChar::Word8->(Color, Color, Char)
word8ToChar x | x < 64 = (Green, Black, regular V.! fromIntegral x)
word8ToChar x | x < 128 = (Black, Green, regular V.! fromIntegral (x - 64))
word8ToChar x | x < 0x90 = (Green, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x | x < 0xa0 = (Yellow, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x | x < 0xb0 = (Blue, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x | x < 0xc0 = (Red, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x | x < 0xd0 = (White, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x | x < 0xe0 = (Blue, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x | x < 0xf0 = (Magenta, Black, block V.! fromIntegral (x `rem` 16))
--word8ToChar x = (Orange, Black, block V.! fromIntegral (x `rem` 16))
word8ToChar x = (Green, Black, block V.! fromIntegral (x `rem` 16))







