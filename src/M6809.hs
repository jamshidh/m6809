{-# LANGUAGE RecordWildCards #-}

module M6809 (
  M6809(..),
  FlagsRegister,
  Flag(..),
  set,
  clear,
  test
  ) where

import Data.Bits
import Data.Default
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Numeric

import Format
import Memory

type FlagsRegister=Word8

data M6809 =
  M6809 {
    pc::Word16,
    nextPC::Maybe Word16,
    a::Word8,
    b::Word8,
    cc::FlagsRegister,
    s::Word16,
    u::Word16,
    x::Word16,
    y::Word16,
    mem::Memory
    } deriving (Show)


set::Flag->FlagsRegister->FlagsRegister
set E = flip setBit 7
set F = flip setBit 6
set H = flip setBit 5
set I = flip setBit 4
set N = flip setBit 3
set Z = flip setBit 2
set V = flip setBit 1
set C = flip setBit 0

clear::Flag->FlagsRegister->FlagsRegister
clear E = flip clearBit 7
clear F = flip clearBit 6
clear H = flip clearBit 5
clear I = flip clearBit 4
clear N = flip clearBit 3
clear Z = flip clearBit 2
clear V = flip clearBit 1
clear C = flip clearBit 0

test::Flag->FlagsRegister->Bool
test E = flip testBit 7
test F = flip testBit 6
test H = flip testBit 5
test I = flip testBit 4
test N = flip testBit 3
test Z = flip testBit 2
test V = flip testBit 1
test C = flip testBit 0

data Flag =
  E | F | H | I | N | Z | V | C


showFlagsRegister::FlagsRegister->String
showFlagsRegister v =
  (if (testBit v 7) then "E" else "-")
  ++ (if (testBit v 6) then "F" else "-")
  ++ (if (testBit v 5) then "H" else "-")
  ++ (if (testBit v 4) then "I" else "-")
  ++ (if (testBit v 3) then "N" else "-")
  ++ (if (testBit v 2) then "Z" else "-")
  ++ (if (testBit v 1) then "V" else "-")
  ++ (if (testBit v 0) then "C" else "-")

                              
                              
{-
Bit 7	E	Entire Flag
Bit 6	F	FIRQ mask Flag
Bit 5	H	Half Carry Flag
Bit 4	I	IRQ Flag
Bit 3	N	Negative sign Flag
Bit 2	Z	Zero Flag
Bit 1	V	Overflow Flag
Bit 0	C	Carry Flag
-}

instance Default M6809 where
  def =
    M6809 {
      pc=0,
      nextPC=Nothing,
      a=0,
      b=0,
      cc=0,
      s=0,
      u=0,
      x=0,
      y=0,
      mem=def
      }

instance Format M6809 where
  format M6809{..} = unlines $
    "M6809:":
    tab (box [
            "pc=" ++ showHex pc "" ++ case nextPC of
                                       Nothing -> ""
                                       Just v -> "(next=" ++ showHex v "" ++ ")",
            "a=" ++ showHex a "",
            "b=" ++ showHex b "",
            "s=" ++ showHex s "",
            "u=" ++ showHex u "",
            "x=" ++ showHex x "",
            "y=" ++ showHex y "",
            "cc=" ++ showFlagsRegister cc,
            "Δmem=" ++ unwords (map (\(a, v) -> "(" ++ showHex a "" ++ "," ++ showHex v "" ++ ")") (diff mem)),
            "stack=[" ++ unwords (map (flip showHex "") (VU.toList $ VU.slice (fromIntegral s) 20 $ base mem)) ++ "....]"
            ])

