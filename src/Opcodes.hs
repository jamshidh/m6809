
module Opcodes (
  Address,
  Reg(..),
  Opcode(..),
  Value(..),
  Base(..),
  bytesToOpcode,
  getOpcodeAt,
  getOpcodesAt
  ) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Numeric

import Format

type ROM = VU.Vector Word8

type Address = Word16

data Pointer =
  Pointer1 Word8
  | Pointer2 Word8 Word8
  | Absolute Word16 deriving (Show)

instance Format Pointer where
  format (Absolute x) = "Abs:0x" ++ showHex x ""
  format x = show x

data Value =
  Value1 Word8
  | Value2 Word8 Word8
  | Direct Word16
  | Relative1 Word8
  | Relative2 Word16
  | Extend Word16
  | Immediate1 Word8
  | Immediate2 Word16
  | IndexInc String Int
  | IndexBase Base String
  | IndexAddr Word16
  | IndexAddress deriving (Show)

toDirect::Word8->Value
toDirect = Direct . fromIntegral

{-
byteToIndex::Word8->Value
byteToIndex v =
  case testBit v 7 of
   False ->
     Index' Nothing (if testBit v 4 then (fromIntegral $ v .&. 31) - 32 else fromIntegral $ v .&. 31) name 0 
   True ->
     case [testBit v 4, testBit v 3, testBit v 2, testBit v 1, testBit v 0] of
      [True, True, False, False, False] -> Index' (Just "7 bit offset") 7 "PC" 0
      [True, True, True, False, False] -> Index' (Just "7 bit offset") 7 "PC" 0
      [True, True, True, True, True] -> IndexAddress
      _ -> Index' other 0 name inc
  where
    name =
      case (testBit v 6, testBit v 5) of
       (False, False) -> "X"
       (False, True) -> "Y"
       (True, False) -> "U"
       (True, True) -> "S"
    (other, inc) = 
      case [testBit v 4, testBit v 3, testBit v 2, testBit v 1, testBit v 0] of
       [False, False, False, False, False] -> (Nothing, 1)
       [False, False, False, True, False] -> (Nothing, -1)
       [False, False, True, False, False] -> (Nothing, 0)
       [True, False, True, False, False] -> (Nothing, 0)
       [False, False, False, False, True] -> (Nothing, 2)
       [False, False, True, True, False] -> (Just "A", 0)
       [False, False, True, False, True] -> (Just "B", 0)
       [True, False, True, False, True] -> (Just "B", 0)
       [False, False, False, True, True] -> (Nothing, -2)
       x -> error $ "missing case in byteToIndex: " ++ show x
-}

data Bit = O | I deriving (Show)

data Base = NumBase Word16 | RegBase String deriving (Show)

toR::(Bit, Bit)->String
toR (O,O) = "X"
toR (O,I) = "Y"
toR (I,O) = "U"
toR (I,I) = "S"

toInt::[Bit]->Int
toInt [] = 0
toInt x =
  let
    nonnegVal = sum $ map (\(i, b) -> case b of; O -> 0; I -> 2^i) $ zip [(0::Int)..] $ reverse x
  in 
   case head x of
    I -> nonnegVal - 2^(length x)
    O -> nonnegVal

{-
toInt [] = 0
toInt (O:rest) = toInt rest
toInt (I:rest) = 2^(length rest)+toInt rest
-}

toWord16::Word8->Word8->Word16
toWord16 x y = fromIntegral x `shiftL` 8 + fromIntegral y



getIndex'::(Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit)->Word8->Word8->(Value, Word16)
getIndex' (O,r1,r2,f1,f2,f3,f4,f5) _ _ =
  (IndexBase (NumBase $ fromIntegral $ toInt [f1, f2, f3, f4, f5]) $ toR (r1, r2), 1)
getIndex' (I,r1,r2,O,O,O,O,O) _ _ = (IndexInc (toR (r1, r2)) 1, 1)
getIndex' (I,r1,r2,_,O,O,O,I) _ _ = (IndexInc (toR (r1, r2)) 2, 1)
getIndex' (I,r1,r2,O,O,O,I,O) _ _ = (IndexInc (toR (r1, r2)) (-1), 1)
getIndex' (I,r1,r2,_,O,O,I,I) _ _ = (IndexInc (toR (r1, r2)) (-2), 1)
getIndex' (I,r1,r2,_,O,I,O,O) _ _ = (IndexInc (toR (r1, r2)) 0, 1)

getIndex' (I,r1,r2,_,O,I,O,I) _ _ = (IndexBase (RegBase "B") $ toR (r1, r2), 1)
getIndex' (I,r1,r2,_,O,I,I,O) _ _ = (IndexBase (RegBase "A") $ toR (r1, r2), 1)
getIndex' (I,_,_,_,O,I,I,I) _ _ = error "illegal index byte"
getIndex' (I,r1,r2,_,I,O,O,O) v _ =
  (IndexBase (NumBase $ fromIntegral v) $ toR (r1, r2), 2)
getIndex' (I,r1,r2,_,I,O,O,I) v1 v2 =
  (IndexBase (NumBase $ toWord16 v1 v2) $ toR (r1, r2), 3)
getIndex' (I,_,_,_,I,O,I,O) _ _ = error "illegal index byte"
getIndex' (I,r1,r2,_,I,O,I,I) _ _ = (IndexBase (RegBase "D") $ toR (r1, r2), 1)

getIndex' (I,_,_,_,I,I,O,O) v _ = (IndexBase (NumBase $ fromIntegral v) "PC", 2)
getIndex' (I,_,_,_,I,I,O,I) v1 v2 = (IndexBase (NumBase $ toWord16 v1 v2) "PC", 3)
getIndex' (I,_,_,_,I,I,I,O) _ _ = error "illegal index byte"
getIndex' (I,O,O,I,I,I,I,I) v1 v2 = (IndexAddr $ toWord16 v1 v2, 3)

getIndex' bits _ _ = error $ "illegal index byte: " ++ show bits


byteToBits::Word8->(Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit)
byteToBits x =
  (getBitAt x 7, getBitAt x 6, getBitAt x 5, getBitAt x 4,
   getBitAt x 3, getBitAt x 2, getBitAt x 1, getBitAt x 0)
  where
    getBitAt::Word8->Int->Bit
    getBitAt v p = if testBit v (fromIntegral p) then I else O



getIndex::[Word8]->(Value, Word16)
getIndex [] = error "getIndex called without any bytes"
getIndex (i:rest) = getIndex' (byteToBits i) v1 v2
  where
    (v1, v2) =
      case rest of
       (f:s:_) -> (f, s)
       [f] -> (f, error "not enough bytes in call to getIndex")
       _ -> (error "not enough bytes in call to getIndex",
             error "not enough bytes in call to getIndex")
       
toTFRReg::Word8->String
toTFRReg 0 = "D"
toTFRReg 1 = "X"
toTFRReg 2 = "Y"
toTFRReg 3 = "U"
toTFRReg 4 = "S"
toTFRReg 5 = "PC"
toTFRReg 8 = "A"
toTFRReg 9 = "B"
toTFRReg 10 = "CCR"
toTFRReg 11 = "DPR"
toTFRReg x = error $ "invalid value in call to toTFRR: " ++ show x

nibble1::Word8->Word8
nibble1 x = x `shiftR` 4 

nibble2::Word8->Word8
nibble2 x = x .&. 0xf
      
instance Format Value where
  format (Direct x) = "D:" ++ showHex x ""
  format (Relative1 x) = "R1:" ++ showHex x ""
  format (Relative2 x) = "R2:" ++ showHex x ""
  format (Extend x) = "L" ++ showHex x ""
  format (Immediate1 x) = "#$" ++ showHex x ""
  format (Immediate2 x) = "#$" ++ showHex x ""
  format (IndexInc reg inc) = "," ++ show reg ++ (if inc < 0 then replicate (-inc) '-' else replicate inc '+')
  format (IndexBase base reg) = show base ++ "," ++ show reg
  format (IndexAddr addr) = "[" ++ showHex addr "" ++ "]"
  format x = show x

toRelative2::Word8->Word8->Value
toRelative2 x y = Relative2 $ fromIntegral x*256 + fromIntegral y

toImmediate2::Word8->Word8->Value
toImmediate2 x y = Immediate2 $ fromIntegral x*256 + fromIntegral y

toExtend::Word8->Word8->Value
toExtend x y = Extend $ fromIntegral x*256 + fromIntegral y

data Something =
  Something1 Word8
  | Something2 Word8 Word8 deriving (Show)

data Reg = CCR | A | B | DPR | X | Y | S | U | PC deriving (Show)

byteToPushRegs::Bool->Word8->[Reg]
byteToPushRegs sNotU x =
  (if testBit x 0 then [CCR] else [])
  ++ (if testBit x 1 then [A] else [])
  ++ (if testBit x 2 then [B] else [])
  ++ (if testBit x 3 then [DPR] else [])
  ++ (if testBit x 4 then [X] else [])
  ++ (if testBit x 5 then [Y] else [])
  ++ (if testBit x 6 then [if sNotU then S else U] else [])
  ++ (if testBit x 7 then [PC] else [])

data Opcode =
  ABX
  | ADCA Value
  | ADCB Value
  | ADDA Value
  | ADDB Value
  | ADDD Value
  | ANDA Value
  | ANDB Value
  | ANDCC Value
  | ASR Value
  | ASRA
  | ASRB
  | BCC Value
  | BCS Value
  | BGE Value
  | BGT Value
  | BEQ Value
  | BHI Value
  | BITA Value
  | BITB Value
  | BLE Value
  | BLS Value
  | BLT Value
  | BMI Value
  | BNE Value
  | BRN Value
  | BPL Value
  | BRA Value
  | BSR Value
  | BVC Value
  | BVS Value
  | CLR Value
  | CLRA
  | CLRB
  | CMPA Value
  | CMPB Value
  | CMPD Value
  | CMPU Value
  | CMPX Value
  | CMPY Value
  | COM Value
  | COMA
  | COMB
  | CWAI Word8
  | DAA
  | DEC Value
  | DECA
  | DECB
  | EORA Value
  | EORB Value
  | EXG String String
  | INC Value
  | INCA
  | INCB
  | JMP Value
  | JSR Value
  | LBCC Value
  | LBCS Value
  | LBEQ Value
  | LBGE Value
  | LBGT Value
  | LBHI Value
  | LBLE Value
  | LBLO Value
  | LBLS Value
  | LBLT Value
  | LBHS Value
  | LBMI Value
  | LBNE Value
  | LBPL Value
  | LBRA Value
  | LBRN Value
  | LBSR Value
  | LBVC Value
  | LBVS Value
  | LEAU Word8
  | LEAX Value
  | LEAS Value
  | LDA Value
  | LDB Value
  | LDD Value
  | LDS Value
  | LDU Value
  | LDX Value
  | LDY Value
  | LEAY Word8 Word8
  | LSL Value
  | LSLA
  | LSLB
  | LSR Value
  | LSRA
  | LSRB
  | MUL
  | NEG Value
  | NEGA
  | NEGB
  | NOP
  | ORA Value
  | ORB Value
  | ORCC Value
  | PSHS [Reg]
  | PSHU [Reg]
  | PULS [Reg]
  | PULU [Reg]
  | RESET
  | ROL Value
  | ROLA
  | ROLB
  | ROR Value
  | RORA
  | RORB
  | RTI
  | RTS
  | SBCA Value
  | SBCB Value
  | SEX
  | STA Value
  | STB Value
  | STD Value
  | STS Value
  | STU Value
  | STX Value
  | STY Value
  | SUBA Value
  | SUBB Value
  | SUBD Value
  | SWI
  | SWI2
  | SYNC
  | TFR String String
  | TST Value
  | TSTA
  | TSTB
  | Page1 Opcode
  | Page2 Opcode
  | Illegal deriving (Show)

instance Format Opcode where
  format (BCC p) = "BCC " ++ format p
  format (BCS p) = "BCS " ++ format p
  format (BGE p) = "BGE " ++ format p
  format (BGT p) = "BGT " ++ format p
  format (BEQ p) = "BEQ " ++ format p
  format (BHI p) = "BHI " ++ format p
  format (BLE p) = "BLE " ++ format p
  format (BLS p) = "BLS " ++ format p
  format (BLT p) = "BLT " ++ format p
  format (BMI p) = "BMI " ++ format p
  format (BNE p) = "BNE " ++ format p
  format (BRN p) = "BRN " ++ format p
  format (BPL p) = "BPL " ++ format p
  format (BRA p) = "BRA " ++ format p
  format (BSR p) = "BSR " ++ format p
  format (BVC p) = "BVC " ++ format p
  format (BVS p) = "BVS " ++ format p
  format (CMPX p) = "CMPX " ++ format p
  format (JMP p) = "JMP " ++ format p
  format (JSR p) = "JSR " ++ format p
  format (SUBA v) = "SUBA " ++ format v
  format (LDA v) = "LDA " ++ format v
  format (LDB v) = "LDB " ++ format v
  format (LDX v) = "LDX " ++ format v
  format (STB v) = "STB " ++ format v
  format (STX v) = "STX " ++ format v
  format (Page1 v) = "Page1 " ++ format v
  format (Page2 v) = "Page2 " ++ format v
  format x = show x
                             
bytesToOpcode::[Word8]->(Opcode, Word16)
bytesToOpcode (0x00:v:_) = (NEG $ toDirect v, 2)
bytesToOpcode (0x01:_) = (Illegal, 1)
bytesToOpcode (0x02:_) = (Illegal, 1)
bytesToOpcode (0x03:v:_) = (COM $ toDirect v, 2)
bytesToOpcode (0x04:v:_) = (LSR $ toDirect v, 2)
bytesToOpcode (0x05:_) = (Illegal, 1)
bytesToOpcode (0x06:v:_) = (ROR $ toDirect v, 2)
bytesToOpcode (0x07:v:_) = (ASR $ toDirect v, 2)
bytesToOpcode (0x08:v:_) = (LSL $ toDirect v, 2)
bytesToOpcode (0x09:v:_) = (ROL $ toDirect v, 2)
bytesToOpcode (0x0a:v:_) = (DEC $ toDirect v, 2)
bytesToOpcode (0x0b:_) = (Illegal, 1)
bytesToOpcode (0x0c:v:_) = (INC $ toDirect v, 2)
bytesToOpcode (0x0d:v:_) = (TST $ toDirect v, 2)
bytesToOpcode (0x0e:v:_) = (JMP $ toDirect v, 2)
bytesToOpcode (0x0f:v:_) = (CLR $ toDirect v, 2)
bytesToOpcode (0x10:0x21:v1:v2:_) = (LBRN $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x22:v1:v2:_) = (LBHI $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x23:v1:v2:_) = (LBLS $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x24:v1:v2:_) = (LBHS $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x25:v1:v2:_) = (LBLO $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x26:v1:v2:_) = (LBNE $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x27:v1:v2:_) = (LBEQ $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x28:v1:v2:_) = (LBVC $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x29:v1:v2:_) = (LBVS $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x2A:v1:v2:_) = (LBPL $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x2B:v1:v2:_) = (LBMI $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x2C:v1:v2:_) = (LBGE $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x2D:v1:v2:_) = (LBLT $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x2E:v1:v2:_) = (LBGT $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x2F:v1:v2:_) = (LBLE $ toRelative2 v1 v2, 4)
bytesToOpcode (0x10:0x3F:_) = (SWI2, 2)
bytesToOpcode (0x10:0x83:v1:v2:_) = (CMPD $ toImmediate2 v1 v2, 4)
bytesToOpcode (0x10:0x8C:v1:v2:_) = (CMPY $ toImmediate2 v1 v2, 4)
bytesToOpcode (0x10:0x8E:v1:v2:_) = (LDY $ toImmediate2 v1 v2, 4)
bytesToOpcode (0x10:0x93:v:_) = (CMPD $ toDirect v, 3)
bytesToOpcode (0x10:0x9C:v:_) = (CMPY $ toDirect v, 3)
bytesToOpcode (0x10:0x9E:v:_) = (LDY $ toDirect v, 3)
bytesToOpcode (0x10:0x9F:v:_) = (STY $ toDirect v, 3)
bytesToOpcode (0x10:0xA3:rest) = let (v, size) = getIndex rest in (CMPD v, 2+size)
--bytesToOpcode (0x10:0xA3:v:_) = (CMPD $ byteToIndex v, 3)
bytesToOpcode (0x10:0xAC:rest) = let (v, size) = getIndex rest in (CMPY v, 2+size)
bytesToOpcode (0x10:0xAE:rest) = let (v, size) = getIndex rest in (LDY v, 2+size)
bytesToOpcode (0x10:0xAF:rest) = let (v, size) = getIndex rest in (STY v, 2+size)
bytesToOpcode (0x10:0xB3:v1:v2:_) = (CMPD $ toExtend v1 v2, 4)
bytesToOpcode (0x10:0xBC:v1:v2:_) = (CMPY $ toExtend v1 v2, 4)
bytesToOpcode (0x10:0xBE:v1:v2:_) = (LDY $ toExtend v1 v2, 4)
bytesToOpcode (0x10:0xBF:v1:v2:_) = (STY $ toExtend v1 v2, 4)
bytesToOpcode (0x10:0xCE:v1:v2:_) = (LDS $ toImmediate2 v1 v2, 4)
bytesToOpcode (0x10:0xDE:v:_) = (LDS $ toDirect v, 3)
bytesToOpcode (0x10:0xDF:v:_) = (STS $ toDirect v, 3)
bytesToOpcode (0x10:0xEE:rest) = let (v, size) = getIndex rest in (LDS v, 2+size)
bytesToOpcode (0x10:0xEF:rest) = let (v, size) = getIndex rest in (STS v, 2+size)
bytesToOpcode (0x10:0xFE:v1:v2:_) = (LDS $ toExtend v1 v2, 4)
bytesToOpcode (0x10:0xFF:v1:v2:_) = (STS $ toExtend v1 v2, 4)
bytesToOpcode (0x10:rest) = (Page1 o, used+1)
                            where (o, used) = bytesToOpcode rest
bytesToOpcode (0x11:0x83:v1:v2:_) = (CMPU $ toImmediate2 v1 v2, 4)
bytesToOpcode (0x11:rest) = (Page2 o, used+1)
                            where (o, used) = bytesToOpcode rest
bytesToOpcode (0x12:_) = (NOP, 1)
bytesToOpcode (0x13:_) = (SYNC, 1)
bytesToOpcode (0x14:_) = (Illegal, 1)
bytesToOpcode (0x15:_) = (Illegal, 1)
bytesToOpcode (0x16:v1:v2:_) = (LBRA $ toRelative2 v1 v2, 3)
bytesToOpcode (0x17:v1:v2:_) = (LBSR $ toRelative2 v1 v2, 3)
bytesToOpcode (0x18:_) = (Illegal, 1)
bytesToOpcode (0x19:_) = (DAA, 1)
bytesToOpcode (0x1a:v:_) = (ORCC $ Immediate1 v, 2)
bytesToOpcode (0x1b:_) = (Illegal, 1)
bytesToOpcode (0x1c:v:_) = (ANDCC $ Immediate1 v, 2)
bytesToOpcode (0x1d:_) = (SEX, 1)
bytesToOpcode (0x1e:v:_) = (EXG (toTFRReg $ nibble1 v) (toTFRReg $ nibble2 v), 2)
bytesToOpcode (0x1f:v:_) = (TFR (toTFRReg $ nibble1 v) (toTFRReg $ nibble2 v), 2)
bytesToOpcode (0x20:v:_) = (BRA $ Relative1 v, 2)
bytesToOpcode (0x21:v:_) = (BRN $ Relative1 v, 2)
bytesToOpcode (0x22:v:_) = (BHI $ Relative1 v, 2)
bytesToOpcode (0x23:v:_) = (BLS $ Relative1 v, 2)
bytesToOpcode (0x24:v:_) = (BCC $ Relative1 v, 2)
bytesToOpcode (0x25:v:_) = (BCS $ Relative1 v, 2)
bytesToOpcode (0x26:v:_) = (BNE $ Relative1 v, 2)
bytesToOpcode (0x27:v:_) = (BEQ $ Relative1 v, 2)
bytesToOpcode (0x28:v:_) = (BVC $ Relative1 v, 2)
bytesToOpcode (0x29:v:_) = (BVS $ Relative1 v, 2)
bytesToOpcode (0x2a:v:_) = (BPL $ Relative1 v, 2)
bytesToOpcode (0x2b:v:_) = (BMI $ Relative1 v, 2)
bytesToOpcode (0x2c:v:_) = (BGE $ Relative1 v, 2)
bytesToOpcode (0x2d:v:_) = (BLT $ Relative1 v, 2)
bytesToOpcode (0x2e:v:_) = (BGT $ Relative1 v, 2)
bytesToOpcode (0x2f:v:_) = (BLE $ Relative1 v, 2)
bytesToOpcode (0x30:rest) = let (v, size) = getIndex rest in (LEAX v, 1+size)
--bytesToOpcode (0x30:0x1f:_) = (LEAX $ Something1 0x1f, 2)
--bytesToOpcode (0x30:0x89:v1:v2:_) = (LEAX $ Something2 v1 v2, 4)
bytesToOpcode (0x31:v1:v2:_) = (LEAY v1 v2, 3)
bytesToOpcode (0x32:rest) = let (v, size) = getIndex rest in (LEAS v, 1+size)
bytesToOpcode (0x33:v:_) = (LEAU v, 2)
bytesToOpcode (0x34:v:_) = (PSHS $ byteToPushRegs False v, 2)
bytesToOpcode (0x35:v:_) = (PULS $ reverse $ byteToPushRegs False v, 2)
bytesToOpcode (0x36:v:_) = (PSHU $ byteToPushRegs True v, 2)
bytesToOpcode (0x37:v:_) = (PSHU $ reverse $ byteToPushRegs True v, 2)
bytesToOpcode (0x38:_) = (Illegal, 1)
bytesToOpcode (0x39:_) = (RTS, 1)
bytesToOpcode (0x3a:_) = (ABX, 1)
bytesToOpcode (0x3b:_) = (RTI, 1)
bytesToOpcode (0x3c:v:_) = (CWAI v, 2)
bytesToOpcode (0x3d:_) = (MUL, 1)
bytesToOpcode (0x3e:_) = (RESET, 1)
bytesToOpcode (0x3f:_) = (SWI, 1)
bytesToOpcode (0x40:_) = (NEGA, 1)
bytesToOpcode (0x41:_) = (Illegal, 1)
bytesToOpcode (0x42:_) = (Illegal, 1)
bytesToOpcode (0x43:_) = (COMA, 1)
bytesToOpcode (0x44:_) = (LSRA, 1)
bytesToOpcode (0x45:_) = (Illegal, 1)
bytesToOpcode (0x46:_) = (RORA, 1)
bytesToOpcode (0x47:_) = (ASRA, 1)
bytesToOpcode (0x48:_) = (LSLA, 1)
bytesToOpcode (0x49:_) = (ROLA, 1)
bytesToOpcode (0x4a:_) = (DECA, 1)
bytesToOpcode (0x4b:_) = (Illegal, 1)
bytesToOpcode (0x4c:_) = (INCA, 1)
bytesToOpcode (0x4d:_) = (TSTA, 1)
bytesToOpcode (0x4e:_) = (Illegal, 1)
bytesToOpcode (0x4f:_) = (CLRA, 1)
bytesToOpcode (0x50:_) = (NEGB, 1)
bytesToOpcode (0x51:_) = (Illegal, 1)
bytesToOpcode (0x52:_) = (Illegal, 1)
bytesToOpcode (0x53:_) = (COMB, 1)
bytesToOpcode (0x54:_) = (LSRB, 1)
bytesToOpcode (0x55:_) = (Illegal, 1)
bytesToOpcode (0x56:_) = (RORB, 1)
bytesToOpcode (0x57:_) = (ASRB, 1)
bytesToOpcode (0x58:_) = (LSLB, 1)
bytesToOpcode (0x59:_) = (ROLB, 1)
bytesToOpcode (0x5a:_) = (DECB, 1)
bytesToOpcode (0x5b:_) = (Illegal, 1)
bytesToOpcode (0x5c:_) = (INCB, 1)
bytesToOpcode (0x5d:_) = (TSTB, 1)
bytesToOpcode (0x5e:_) = (Illegal, 1)
bytesToOpcode (0x5f:_) = (CLRB, 1)
bytesToOpcode (0x60:rest) = let (v, size) = getIndex rest in (NEG v, 1+size)
bytesToOpcode (0x61:_) = (Illegal, 1)
bytesToOpcode (0x62:_) = (Illegal, 1)
bytesToOpcode (0x63:rest) = let (v, size) = getIndex rest in (COM  v, 1+size)
bytesToOpcode (0x64:rest) = let (v, size) = getIndex rest in (LSR v, 1+size)
bytesToOpcode (0x65:_) = (Illegal, 1)
bytesToOpcode (0x66:rest) = let (v, size) = getIndex rest in (ROR v, 1+size)
bytesToOpcode (0x67:rest) = let (v, size) = getIndex rest in (ASR v, 1+size)
bytesToOpcode (0x68:rest) = let (v, size) = getIndex rest in (LSL v, 1+size)
bytesToOpcode (0x69:rest) = let (v, size) = getIndex rest in (ROL v, 1+size)
bytesToOpcode (0x6a:rest) = let (v, size) = getIndex rest in (DEC v, 1+size)
bytesToOpcode (0x6b:_) = (Illegal, 1)
bytesToOpcode (0x6c:rest) = let (v, size) = getIndex rest in (INC v, 1+size)
bytesToOpcode (0x6d:rest) = let (v, size) = getIndex rest in (TST v, 1+size)
bytesToOpcode (0x6e:rest) = let (v, size) = getIndex rest in (JMP v, 1+size)
bytesToOpcode (0x6f:rest) = let (v, size) = getIndex rest in (CLR v, 1+size)
bytesToOpcode (0x70:v1:v2:_) = (NEG $ toExtend v1 v2, 3)
bytesToOpcode (0x71:_) = (Illegal, 1)
bytesToOpcode (0x72:_) = (Illegal, 1)
bytesToOpcode (0x73:v1:v2:_) = (COM $ toExtend v1 v2, 3)
bytesToOpcode (0x74:v1:v2:_) = (LSR $ toExtend v1 v2, 3)
bytesToOpcode (0x75:_) = (Illegal, 1)
bytesToOpcode (0x76:v1:v2:_) = (ROR $ toExtend v1 v2, 3)
bytesToOpcode (0x77:v1:v2:_) = (ASR $ toExtend v1 v2, 3)
bytesToOpcode (0x78:v1:v2:_) = (LSL $ toExtend v1 v2, 3)
bytesToOpcode (0x79:v1:v2:_) = (ROL $ toExtend v1 v2, 3)
bytesToOpcode (0x7a:v1:v2:_) = (DEC $ toExtend v1 v2, 3)
bytesToOpcode (0x7b:_) = (Illegal, 1)
bytesToOpcode (0x7c:v1:v2:_) = (TST $ toExtend v1 v2, 3)
bytesToOpcode (0x7d:v1:v2:_) = (TST $ toExtend v1 v2, 3)
bytesToOpcode (0x7e:v1:v2:_) = (JMP $ toExtend v1 v2, 3)
bytesToOpcode (0x7f:v1:v2:_) = (CLR $ toExtend v1 v2, 3)
bytesToOpcode (0x80:v:_) = (SUBA $ Immediate1 v, 2)
bytesToOpcode (0x81:v:_) = (CMPA $ Immediate1 v, 2)
bytesToOpcode (0x82:v:_) = (SBCA $ Immediate1 v, 2)
bytesToOpcode (0x83:v1:v2:_) = (SUBD $ toImmediate2 v1 v2, 2)
bytesToOpcode (0x84:v:_) = (ANDA $ Immediate1 v, 2)
bytesToOpcode (0x85:v:_) = (BITA $ Immediate1 v, 2)
bytesToOpcode (0x86:v:_) = (LDA $ Immediate1 v, 2)
bytesToOpcode (0x87:_) = (Illegal, 1)
bytesToOpcode (0x88:v:_) = (EORA $ Immediate1 v, 2)
bytesToOpcode (0x89:v:_) = (ADCA $ Immediate1 v, 2)
bytesToOpcode (0x8a:v:_) = (ORA $ Immediate1 v, 2)
bytesToOpcode (0x8b:v:_) = (ADDA $ Immediate1 v, 2)
bytesToOpcode (0x8c:v1:v2:_) = (CMPX $ toImmediate2 v1 v2, 3)
bytesToOpcode (0x8d:v:_) = (BSR $ Relative1 v, 2)
bytesToOpcode (0x8e:v1:v2:_) = (LDX $ toImmediate2 v1 v2, 3)
bytesToOpcode (0x8f:_) = (Illegal, 1)
bytesToOpcode (0x90:v:_) = (SUBA $ toDirect v, 2)
bytesToOpcode (0x91:v:_) = (CMPA $ toDirect v, 2)
bytesToOpcode (0x92:v:_) = (SBCA $ toDirect v, 2)
bytesToOpcode (0x93:v:_) = (SUBD $ toDirect v, 2)
bytesToOpcode (0x94:v:_) = (ANDA $ toDirect v, 2)
bytesToOpcode (0x95:v:_) = (BITA $ toDirect v, 2)
bytesToOpcode (0x96:v:_) = (LDA $ toDirect v, 2)
bytesToOpcode (0x97:v:_) = (STA $ toDirect v, 2)
bytesToOpcode (0x98:v:_) = (EORA $ toDirect v, 2)
bytesToOpcode (0x99:v:_) = (ADCA $ toDirect v, 2)
bytesToOpcode (0x9a:v:_) = (ORA $ toDirect v, 2)
bytesToOpcode (0x9b:v:_) = (ADDA $ toDirect v, 2)
bytesToOpcode (0x9c:v:_) = (CMPX $ toDirect v, 2)
bytesToOpcode (0x9d:v:_) = (JSR $ toDirect v, 2)
bytesToOpcode (0x9e:v:_) = (LDX $ toDirect v, 2)
bytesToOpcode (0x9f:v:_) = (STX $ toDirect v, 2)
bytesToOpcode (0xa0:rest) = let (v, size) = getIndex rest in (SUBA v, 1+size)
bytesToOpcode (0xa1:rest) = let (v, size) = getIndex rest in (CMPA v, 1+size)
bytesToOpcode (0xa2:rest) = let (v, size) = getIndex rest in (SBCA v, 1+size)
bytesToOpcode (0xa3:rest) = let (v, size) = getIndex rest in (SUBD v, 1+size)
bytesToOpcode (0xa4:rest) = let (v, size) = getIndex rest in (ANDA v, 1+size)
bytesToOpcode (0xa5:rest) = let (v, size) = getIndex rest in (BITA v, 1+size)
bytesToOpcode (0xa6:rest) = let (v, size) = getIndex rest in (LDA v, 1+size)
bytesToOpcode (0xa7:rest) = let (v, size) = getIndex rest in (STA v, 1+size)
bytesToOpcode (0xa8:rest) = let (v, size) = getIndex rest in (EORA v, 1+size)
bytesToOpcode (0xa9:rest) = let (v, size) = getIndex rest in (ADCA v, 1+size)
bytesToOpcode (0xaa:rest) = let (v, size) = getIndex rest in (ORA  v, 1+size)
bytesToOpcode (0xab:rest) = let (v, size) = getIndex rest in (ADDA v, 1+size)
bytesToOpcode (0xac:rest) = let (v, size) = getIndex rest in (CMPX v, 1+size)
bytesToOpcode (0xad:rest) = let (v, size) = getIndex rest in (JSR v, 1+size)
bytesToOpcode (0xae:rest) = let (v, size) = getIndex rest in (LDX v, 1+size)
bytesToOpcode (0xaf:rest) = let (v, size) = getIndex rest in (STX v, 1+size) 
bytesToOpcode (0xb0:v1:v2:_) = (SUBA $ toExtend v1 v2, 3)
bytesToOpcode (0xb1:v1:v2:_) = (CMPA $ toExtend v1 v2, 3)
bytesToOpcode (0xb2:v1:v2:_) = (SBCA $ toExtend v1 v2, 3)
bytesToOpcode (0xb3:v1:v2:_) = (SUBD $ toExtend v1 v2, 3)
bytesToOpcode (0xb4:v1:v2:_) = (ANDA $ toExtend v1 v2, 3)
bytesToOpcode (0xb5:v1:v2:_) = (BITA $ toExtend v1 v2, 3)
bytesToOpcode (0xb6:v1:v2:_) = (LDA $ toExtend v1 v2, 3)
bytesToOpcode (0xb7:v1:v2:_) = (STA $ toExtend v1 v2, 3)
bytesToOpcode (0xb8:v1:v2:_) = (EORA $ toExtend v1 v2, 3)
bytesToOpcode (0xb9:v1:v2:_) = (ADCA $ toExtend v1 v2, 3)
bytesToOpcode (0xba:v1:v2:_) = (ORA $ toExtend v1 v2, 3)
bytesToOpcode (0xbb:v1:v2:_) = (ADDA $ toExtend v1 v2, 3)
bytesToOpcode (0xbc:v1:v2:_) = (CMPX $ toExtend v1 v2, 3)
bytesToOpcode (0xbd:v1:v2:_) = (JSR $ toExtend v1 v2, 3)
bytesToOpcode (0xbe:v1:v2:_) = (LDX $ toExtend v1 v2, 3)
bytesToOpcode (0xbf:v1:v2:_) = (STX $ toExtend v1 v2, 3)
bytesToOpcode (0xc0:v:_) = (SUBB $ Immediate1 v, 2)
bytesToOpcode (0xc1:v:_) = (CMPB $ Immediate1 v, 2)
bytesToOpcode (0xc2:v:_) = (SBCB $ Immediate1 v, 2)
bytesToOpcode (0xc3:v1:v2:_) = (ADDD $ toImmediate2 v1 v2, 2)
bytesToOpcode (0xc4:v:_) = (ANDB $ Immediate1 v, 2)
bytesToOpcode (0xc5:v:_) = (BITB $ Immediate1 v, 2)
bytesToOpcode (0xc6:v:_) = (LDB $ Immediate1 v, 2)
bytesToOpcode (0xc7:_) = (Illegal, 1)
bytesToOpcode (0xc8:v:_) = (EORB $ Immediate1 v, 2)
bytesToOpcode (0xc9:v:_) = (ADCB $ Immediate1 v, 2)
bytesToOpcode (0xca:v:_) = (ORB $ Immediate1 v, 2)
bytesToOpcode (0xcb:v:_) = (ADDB $ Immediate1 v, 2)
bytesToOpcode (0xcc:v1:v2:_) = (LDD $ toImmediate2 v1 v2, 3)
bytesToOpcode (0xcd:_) = (Illegal, 1)
bytesToOpcode (0xce:v1:v2:_) = (LDU $ toImmediate2 v1 v2, 3)
bytesToOpcode (0xcf:_) = (Illegal, 1)
bytesToOpcode (0xd0:v:_) = (SUBB $ toDirect v, 2)
bytesToOpcode (0xd1:v:_) = (CMPB $ toDirect v, 2)
bytesToOpcode (0xd2:v:_) = (SBCB $ toDirect v, 2)
bytesToOpcode (0xd3:v:_) = (ADDD $ toDirect v, 2)
bytesToOpcode (0xd4:v:_) = (ANDB $ toDirect v, 2)
bytesToOpcode (0xd5:v:_) = (BITB $ toDirect v, 2)
bytesToOpcode (0xd6:v:_) = (LDB $ toDirect v, 2)
bytesToOpcode (0xd7:v:_) = (STB $ toDirect v, 2)
bytesToOpcode (0xd8:v:_) = (EORB $ toDirect v, 2)
bytesToOpcode (0xd9:v:_) = (ADCB $ toDirect v, 2)
bytesToOpcode (0xda:v:_) = (ORB $ toDirect v, 2)
bytesToOpcode (0xdb:v:_) = (ADDB $ toDirect v, 2)
bytesToOpcode (0xdc:v:_) = (LDD $ toDirect v, 2)
bytesToOpcode (0xdd:v:_) = (STD $ toDirect v, 2)
bytesToOpcode (0xde:v:_) = (LDU $ toDirect v, 2)
bytesToOpcode (0xdf:v:_) = (STU $ toDirect v, 2)
bytesToOpcode (0xe0:rest) = let (o, size) = getIndex rest in (SUBB o, 1+size)
bytesToOpcode (0xe1:rest) = let (o, size) = getIndex rest in (CMPB o, 1+size)
bytesToOpcode (0xe2:rest) = let (o, size) = getIndex rest in (SBCB o, 1+size)
bytesToOpcode (0xe3:rest) = let (o, size) = getIndex rest in (ADDD o, 1+size)
bytesToOpcode (0xe4:rest) = let (o, size) = getIndex rest in (ANDB o, 1+size)
bytesToOpcode (0xe5:rest) = let (o, size) = getIndex rest in (BITB o, 1+size)
bytesToOpcode (0xe6:rest) = let (o, size) = getIndex rest in (LDB o, 1+size)
bytesToOpcode (0xe7:rest) = let (o, size) = getIndex rest in (STB o, 1+size)
bytesToOpcode (0xe8:rest) = let (o, size) = getIndex rest in (EORB o, 1+size)
bytesToOpcode (0xe9:rest) = let (o, size) = getIndex rest in (ADCB o, 1+size)
bytesToOpcode (0xea:rest) = let (o, size) = getIndex rest in (ORB o, 1+size)
bytesToOpcode (0xeb:rest) = let (o, size) = getIndex rest in (ADDB o, 1+size)
bytesToOpcode (0xec:rest) = let (o, size) = getIndex rest in (LDD o, 1+size)
bytesToOpcode (0xed:rest) = let (o, size) = getIndex rest in (STD o, 1+size)
bytesToOpcode (0xee:rest) = let (o, size) = getIndex rest in (LDU o, 1+size)
bytesToOpcode (0xef:rest) = let (o, size) = getIndex rest in (STU o, 1+size)
bytesToOpcode (0xf0:v1:v2:_) = (SUBB $ toExtend v1 v2, 3)
bytesToOpcode (0xf1:v1:v2:_) = (CMPB $ toExtend v1 v2, 3)
bytesToOpcode (0xf2:v1:v2:_) = (SBCB $ toExtend v1 v2, 3)
bytesToOpcode (0xf3:v1:v2:_) = (ADDD $ toExtend v1 v2, 3)
bytesToOpcode (0xf4:v1:v2:_) = (ANDB $ toExtend v1 v2, 3)
bytesToOpcode (0xf5:v1:v2:_) = (BITB $ toExtend v1 v2, 3)
bytesToOpcode (0xf6:v1:v2:_) = (LDB $ toExtend v1 v2, 3)
bytesToOpcode (0xf7:v1:v2:_) = (STB $ toExtend v1 v2, 3)
bytesToOpcode (0xf8:v1:v2:_) = (EORB $ toExtend v1 v2, 3)
bytesToOpcode (0xf9:v1:v2:_) = (ADCB $ toExtend v1 v2, 3)
bytesToOpcode (0xfa:v1:v2:_) = (ORB $ toExtend v1 v2, 3)
bytesToOpcode (0xfb:v1:v2:_) = (ADDB $ toExtend v1 v2, 3)
bytesToOpcode (0xfc:v1:v2:_) = (LDD $ toExtend v1 v2, 3)
bytesToOpcode (0xfd:v1:v2:_) = (STD $ toExtend v1 v2, 3)
bytesToOpcode (0xfe:v1:v2:_) = (LDU $ toExtend v1 v2, 3)
bytesToOpcode (0xff:v1:v2:_) = (STU $ toExtend v1 v2, 3)

bytesToOpcode v =
  case v of
   [] -> error "bytesToOpcode called with empty string"
   (x:rest) -> error $ "unknown opcode: 0x" ++ showHex x "" ++ "    " ++ BC.unpack (B16.encode $ B.pack $ take 4 $ rest)

getOpcodeAt::ROM->Address->(Opcode, Word16)
getOpcodeAt rom p = bytesToOpcode (VU.toList $ VU.drop (fromIntegral p) rom)
  
--getOPCode::Int->B.ByteString
--getOPCode p = 

getOpcodesAt::ROM->Address->Word16->[(Address, B.ByteString, Opcode)]
getOpcodesAt _ _ 0 = []
getOpcodesAt rom p numberOfOpcodes =
  (p, B.pack $ VU.toList $ VU.slice (fromIntegral p) (fromIntegral used) rom, o):getOpcodesAt rom (p+used) (numberOfOpcodes - 1)
  where
    (o, used) = getOpcodeAt rom p

