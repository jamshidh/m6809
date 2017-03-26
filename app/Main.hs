{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import HFlags
import Numeric

import Format
import Opcodes
import Options ()
import VM


main :: IO ()
main = do
  _ <- $initHFlags "Color Computer Simulator"
  cocoRoms <- fmap (B.drop 2) $ B.readFile "COCO.ROM"
  let rom =
        B.replicate 0x8000 0
        `B.append` cocoRoms
        `B.append` B.replicate (0x8000-B.length cocoRoms) 0
        
  rom' <- VU.unsafeThaw $ VU.fromList $ B.unpack rom::IO (VUM.IOVector Word8)
  let startP = 0xa00e --seems to be start of the program
  
  frozenRom <- VU.unsafeFreeze rom'
  let (o', size) = getOpcodeAt frozenRom startP

  run rom' startP

