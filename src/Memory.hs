
module Memory (
  Memory(..),
  peek,
  poke,
  peek2,
  poke2
  ) where

import Data.Bits
import Data.Default
import qualified Data.Vector.Unboxed as V
import Data.Word

data Memory =
  Memory {
    base::V.Vector Word8,
    diff::[(Word16, Word8)]
    } deriving (Show)

instance Default Memory where
  def =
    Memory {
      base = V.replicate 65536 0,
      diff = []
      }

peek::Memory->Word16->Word8
peek mem p = base mem V.! fromIntegral p

poke::Memory->Word16->Word8->Memory
poke mem p v = mem{diff=(p, v):diff mem}

peek2::Memory->Word16->Word16
peek2 mem p = fromIntegral (peek mem p) `shiftL` 8 + fromIntegral (peek mem (p+1))

poke2::Memory->Word16->Word16->Memory
poke2 mem p v = poke (poke mem p (fromIntegral $ v `shiftR` 8)) (p+1) (fromIntegral v)

