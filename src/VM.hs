{-# LANGUAGE RecordWildCards #-}

module VM (
  run
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as B
import Data.Default
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Numeric
import System.Console.ANSI

import CharSet
import Format
import M6809
import Memory
import Opcodes
import Options

getKeyPresses::TChan Char->IO ()
getKeyPresses keyChan = forever $ do
  threadDelay 5000000
  forM "LIST\n" $ \c -> do
    atomically $ writeTChan keyChan c
    threadDelay 2000000

run::VUM.IOVector Word8->Address->IO ()
run rom location = do
  let m6809 = def{pc = location}
  keyChan <- newTChanIO
  forkIO $ getKeyPresses keyChan
  run' keyChan 'A' rom m6809

keyRow::Char->Int
keyRow '\n' = 0
keyRow c = fromIntegral $ (B.c2w c - 64) .&. 0x7

keyCol::Char->Int
keyCol '\n' = 6
keyCol c = fromIntegral $ (B.c2w c - 64) `shiftR` 3

run'::TChan Char->Char->VUM.IOVector Word8->M6809->IO ()
run' keyChan keyIn rom m6809 = do
  frozenMem <- VU.unsafeFreeze rom

  --putStrLn $ "screen: " ++ show (B.pack $ VU.toList $ VU.slice 0x1000 100 $ frozenMem)

  setCursorPosition 0 0
{-  forM_ [0, 32..15*32] $ \i -> 
    putStrLn $
          setSGRCode[SetColor Foreground Dull Black, SetColor Background Vivid Green]
          ++ (map word8ToChar $ VU.toList $ VU.slice (0x400+i) 32 $ frozenMem)
          ++ setSGRCode [Reset]
  -}  

  let (opcode, size) = getOpcodeAt frozenMem (pc m6809)
      m6809' = runOpcode m6809{mem=Memory{base=frozenMem, diff=[]}} opcode

  setCursorPosition 18 0
  --clearFromCursorToScreenEnd
  when flags_debug $ do
    putStr $ "#### " ++ showHex (pc m6809) " "
    putStr $ BC.unpack $ B16.encode $ B.pack $ VU.toList $ VU.slice (fromIntegral $ pc m6809) (fromIntegral size) frozenMem
    putStrLn $ " " ++ format opcode
    putStrLn $ format m6809'

  putStrLn $ format m6809'

  let pc' =
        case nextPC m6809' of
         Nothing -> size + pc m6809'
         Just v -> v
  forM_ (diff $ mem m6809') $ \(addr, val) -> do
    when (addr >= 0xa000 && addr < 0xff00) $ error "trying to set memory in rom"
    VUM.write rom (fromIntegral addr) val
    when (addr == 0xff02) $
--      if val == 0xdf
      if val == clearBit 0xff (keyRow keyIn) -- clearBit 0xff 1 --5
        then do
          --error $ "setting " ++ showHex addr "" ++ " to " ++ show val
          VUM.write rom 0xff00 $ clearBit 0xff $ keyCol keyIn
--        when (addr >= 0xff00 && addr <= 0xff04) $ error $ "setting " ++ showHex addr "" ++ " to " ++ show val
        else VUM.write rom 0xff00 0xff
    when (addr >= 0x400 && addr <= 0x400+32*16 - 1) $ do
      let x = fromIntegral $ (addr - 0x400) .&. 0x1f
          y = fromIntegral $ (addr - 0x400) `div` 32
      setCursorPosition y x
      let (fgColor, bgColor, char) = word8ToChar val
      putStr $ setSGRCode
                  [
                    SetColor Foreground Dull fgColor,
                    SetColor Background Vivid bgColor
                  ]
      putChar char
      putStr $ setSGRCode [Reset]

  isNewKey <- fmap not $ atomically $ isEmptyTChan keyChan

  newKeyIn <-
    if isNewKey
    then atomically $ readTChan keyChan
--      return $ if newVal then 0xff else clearBit 0xff 0 --6
    else return keyIn
      
  run' keyChan newKeyIn rom m6809'{pc=pc', mem=(mem m6809'){diff=[]}, nextPC=Nothing}


step::Value->M6809->M6809
step (Direct _) m6809 = m6809
step (Extend _) m6809 = m6809
step (Immediate1 _) m6809 = m6809
step (IndexAddr _) m6809 = m6809
step (IndexBase _ _) m6809 = m6809
step (IndexInc "S" diff) m6809@M6809{..} = m6809{s=s+fromIntegral diff}
step (IndexInc "U" diff) m6809@M6809{..} = m6809{u=u+fromIntegral diff}
step (IndexInc "X" diff) m6809@M6809{..} = m6809{x=x+fromIntegral diff}
step v _ = error $ "step doesn't yet support " ++ show v

getAddrFromValue::M6809->Value->Word16
getAddrFromValue _ (Direct v) = v
getAddrFromValue _ (Extend v) = v
getAddrFromValue M6809{..} (IndexInc "S" _) = s
getAddrFromValue M6809{..} (IndexInc "U" _) = u
getAddrFromValue M6809{..} (IndexInc "X" _) = x
getAddrFromValue M6809{..} (IndexBase (NumBase v) "S") = v+s
getAddrFromValue M6809{..} (IndexBase (NumBase v) "U") = v+u
getAddrFromValue M6809{..} (IndexBase (NumBase v) "X") = v+x
getAddrFromValue M6809{..} (IndexBase (RegBase "B") "X") = fromIntegral b+x
getAddrFromValue M6809{..} (IndexAddr p) = peek2 mem p
getAddrFromValue _ v = error $ "getAddrFromValue doesn't yet support " ++ show v

getWord8FromValue::M6809->Value->Word8
getWord8FromValue M6809{..} (Direct v) = peek mem v
getWord8FromValue M6809{..} (Extend v) = peek mem v
getWord8FromValue _ (Immediate1 v) = v
getWord8FromValue M6809{..} (IndexBase (NumBase v) "S") = peek mem $ v+s
getWord8FromValue M6809{..} (IndexBase (NumBase v) "U") = peek mem $ v+u
getWord8FromValue M6809{..} (IndexBase (NumBase v) "X") = peek mem $ v+x
getWord8FromValue M6809{..} (IndexBase (NumBase v) "Y") = peek mem $ v+y
getWord8FromValue M6809{..} (IndexBase (RegBase "B") "S") = peek mem $ fromIntegral b+s
getWord8FromValue M6809{..} (IndexBase (RegBase "B") "U") = peek mem $ fromIntegral b+u
getWord8FromValue M6809{..} (IndexBase (RegBase "B") "X") = peek mem $ fromIntegral b+x
getWord8FromValue M6809{..} (IndexBase (RegBase "B") "Y") = peek mem $ fromIntegral b+y
getWord8FromValue M6809{..} (IndexInc "S" _) = peek mem s
getWord8FromValue M6809{..} (IndexInc "U" _) = peek mem u
getWord8FromValue M6809{..} (IndexInc "X" _) = peek mem x
getWord8FromValue _ v = error $ "getWord8FromValue doesn't yet support " ++ show v



getWord16FromValue::M6809->Value->Word16
getWord16FromValue M6809{..} (Direct v) = peek2 mem v
getWord16FromValue M6809{..} (Extend v) = peek2 mem v
getWord16FromValue M6809{..} (IndexBase (NumBase v) "S") = peek2 mem $ v+s
getWord16FromValue M6809{..} (IndexBase (NumBase v) "X") = peek2 mem $ v+x
getWord16FromValue _ v = error $ "getWord16FromValue doesn't yet support " ++ show v

  
cmp::(Ord a, Bounded a, Num a)=>FlagsRegister->a->a->FlagsRegister
cmp cc x y = cc''
  where
    cc' = case x `compare` y of
           LT -> set N $ clear Z $ set C cc
           EQ -> clear N $ set Z $ clear C cc
           GT -> clear N $ clear Z $ clear C cc
--           LT -> set N $ clear Z $ clear C cc
--           EQ -> clear N $ set Z $ clear C cc
--           GT -> clear N $ clear Z $ set C cc
    cc'' = case x > maxBound + y of
      True -> set V cc'
      False -> clear V cc'



runOpcode::M6809->Opcode->M6809

runOpcode m6809@M6809{..} ABX = m6809{x=x+fromIntegral b}

runOpcode m6809@M6809{..} (ADCA v) =
  step v $ m6809{a=a+getWord8FromValue m6809 v + (if test C cc then 1 else 0)}

runOpcode m6809@M6809{..} (ADDA v) = step v $ m6809{a=a+getWord8FromValue m6809 v}
runOpcode m6809@M6809{..} (ADDB v) = step v $ m6809{b=b+getWord8FromValue m6809 v}

runOpcode m6809@M6809{..} (ADDD v) =
  step v $ m6809{a=a', b=b'}
  where
    d = fromIntegral a `shiftL` 8 + fromIntegral b
    d' = d + getWord16FromValue m6809 v
    a' = fromIntegral $ d' `shiftR` 8
    b' = fromIntegral d'

runOpcode m6809@M6809{..} (ANDA v) =
  step v $ m6809{a=a .&. getWord8FromValue m6809 v}

runOpcode m6809@M6809{..} (ANDB v) =
  step v $ m6809{b=b .&. getWord8FromValue m6809 v}

runOpcode m6809@M6809{..} (ANDCC v) =
  step v $ m6809{cc=cc .&. getWord8FromValue m6809 v}


runOpcode m6809@M6809{..} (BITA (Immediate1 arg)) =
  m6809{cc=
        (if v == False then set Z else clear Z)
        $ (if v == True then set N else clear N)
        $ clear V cc}
  where
    v = a .&. fromIntegral arg /= 0

runOpcode m6809@M6809{..} (BITB (Immediate1 arg)) =
  m6809{cc=
        (if v == False then set Z else clear Z)
        $ (if v == True then set N else clear N)
        $ clear V cc}
  where
    v = b .&. fromIntegral arg /= 0


runOpcode m6809@M6809{..} (BCC (Relative1 v)) =
  case not $ test C cc of
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}
   False -> m6809

runOpcode m6809@M6809{..} (BCS (Relative1 v)) =
  case test C cc of
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}
   False -> m6809

--TODO verify these are the conditions
runOpcode m6809@M6809{..} (BHI (Relative1 v)) =
  case test C cc || test Z cc of
   True -> m6809
   False -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}

runOpcode m6809@M6809{..} (BLE (Relative1 v)) =
  case test Z cc || (test N cc && test V cc) of
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}
   False -> m6809

runOpcode m6809@M6809{..} (LBLE (Relative2 v)) =
  case test Z cc || (test N cc && test V cc) of
   True -> m6809{pc=v + pc}
   False -> m6809


{-runOpcode m6809@M6809{..} (BLO (Relative1 v)) =
  case test C cc of
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}
   False -> m6809
-}

runOpcode m6809@M6809{..} (LBLO (Relative2 v)) =
  case test C cc of
   True -> m6809{pc=v + pc}
   False -> m6809


runOpcode m6809@M6809{..} (BLS (Relative1 v)) =
  case test C cc || test Z cc of
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}
   False -> m6809

runOpcode m6809@M6809{..} (BMI (Relative1 v)) =
  case test N cc of
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}
   False -> m6809

runOpcode m6809@M6809{..} (BNE (Relative1 v)) =
  case test Z cc of
   True -> m6809
   False -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}

runOpcode m6809@M6809{..} (LBNE (Relative2 v)) =
  case test Z cc of
   True -> m6809
   False -> m6809{pc=v + pc}


runOpcode m6809@M6809{..} (BSR (Relative1 v)) =
   m6809{s=s-2, mem=poke2 mem (s-2) (pc+2),
         pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}

runOpcode m6809@M6809{..} (BRA (Relative1 v)) =
   m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}

runOpcode m6809@M6809{..} (BEQ (Relative1 v)) =
  case test Z cc of
   False -> m6809
   True -> m6809{pc=fromIntegral v + (if testBit v 7 then (-256) else 0) + pc}

runOpcode m6809@M6809{..} (LBEQ (Relative2 v)) =
  case test Z cc of
   False -> m6809
   True -> m6809{pc=v + pc}










-------
runOpcode m6809@M6809{..} CLRA =
  m6809{a=0, cc=clear N $ set Z $ clear V $ clear C cc}
runOpcode m6809@M6809{..} CLRB =
  m6809{b=0, cc=clear N $ set Z $ clear V $ clear C cc}

runOpcode m6809@M6809{..} (CLR v) =
  step v $ m6809{mem=poke mem (getAddrFromValue m6809 v) 0,
                 cc=clear N $ set Z $ clear V $ clear C cc}

-------


runOpcode m6809@M6809{..} (CMPA v) =
  step v $ m6809{cc=cmp cc a $ getWord8FromValue m6809 v}

runOpcode m6809@M6809{..} (CMPB v) =
  step v $ m6809{cc=cmp cc b $ getWord8FromValue m6809 v}



runOpcode m6809@M6809{..} (CMPD (Direct p)) =
  m6809{cc=cmp cc d $ peek2 mem p}
  where
    d=fromIntegral a `shiftL` 8 + fromIntegral b

runOpcode m6809@M6809{..} (CMPU (Immediate2 v)) = m6809{cc=cmp cc u v}



runOpcode m6809@M6809{..} (CMPX (Immediate2 v)) = m6809{cc=cmp cc x v}
--           LT -> set N $ clear Z $ set C cc
--           EQ -> clear N $ set Z $ clear C cc
--           GT -> clear N $ clear Z $ clear C cc

runOpcode m6809@M6809{..} (CMPX (Direct p)) = m6809{cc=cmp cc x $ peek2 mem p}

runOpcode m6809@M6809{..} (CMPX (Extend p)) = m6809{cc=cmp cc x $ peek2 mem p}



runOpcode m6809@M6809{..} COMA = m6809{a=complement a}
runOpcode m6809@M6809{..} COMB = m6809{b=complement b}








runOpcode m6809@M6809{..} (DEC (Direct v)) = m6809{mem=poke mem v (peek mem v - 1)}

runOpcode m6809@M6809{..} DECA =
  m6809{a=a-1, cc=cc'}
  where
    cc' = if a-1 == 0
          then set Z cc
          else clear Z cc

runOpcode m6809@M6809{..} DECB =
  m6809{b=b-1, cc=cc'}
  where
    cc' = if b-1 == 0
          then set Z cc
          else clear Z cc
-- TODO need to set these
--    N
--    V

runOpcode m6809@M6809{..} (EORA (Immediate1 v)) = m6809{a=a `xor` v}
runOpcode m6809@M6809{..} (EORA (IndexInc "X" v)) =
  m6809{x=x+fromIntegral v, a=a `xor` peek mem x}

runOpcode m6809@M6809{..} (EORB (Immediate1 v)) = m6809{b=b `xor` v}
runOpcode m6809@M6809{..} (EORB (IndexInc "X" v)) =
  m6809{x=x+fromIntegral v, b=b `xor` peek mem x}

runOpcode m6809 (EXG "A" "A") = m6809
runOpcode m6809@M6809{..} (EXG "A" "B") = m6809{a=b, b=a}



runOpcode m6809@M6809{..} INCA = m6809{a=a+1, cc=cc''''}
  where
    cc' = if testBit (b + 1) 7
          then set N cc
          else clear N cc
    cc'' = if b + 1 == 0 
           then set Z cc'
           else clear Z cc'
    cc''' = if b == 0xff 
            then set C cc''
            else clear C cc''
    cc'''' = if b == 0x7f
             then set V cc'''
             else clear V cc'''
runOpcode m6809@M6809{..} INCB =
  m6809{b=b+1, cc=cc''''}
  where
    cc' = if testBit (b + 1) 7
          then set N cc
          else clear N cc
    cc'' = if b + 1 == 0 
           then set Z cc'
           else clear Z cc'
    cc''' = if b == 0xff 
            then set C cc''
            else clear C cc''
    cc'''' = if b == 0x7f
             then set V cc'''
             else clear V cc'''


runOpcode m6809@M6809{..} (INC (Direct v)) = m6809{mem=poke mem v (peek mem v + 1)}
runOpcode m6809@M6809{..} (INC (IndexBase (NumBase v) "S")) =
  m6809{mem=poke mem (v+s) (peek mem (v+s) + 1)}
runOpcode m6809@M6809{..} (INC (IndexBase (NumBase v) "X")) =
  m6809{mem=poke mem (v+x) (peek mem (v+x) + 1)}




runOpcode m6809@M6809{..} (JMP (Extend v)) =
   m6809{nextPC=Just v}
runOpcode m6809@M6809{..} (JMP (IndexInc "U" v)) = 
   m6809{u=u+fromIntegral v, nextPC=Just u}
runOpcode m6809@M6809{..} (JMP (IndexInc "X" v)) = 
   m6809{x=x+fromIntegral v, nextPC=Just x}

runOpcode m6809@M6809{..} (JSR (Direct v)) =
   m6809{s=s-2, mem=poke2 mem (s-2) (pc+2), nextPC=Just v}
runOpcode m6809@M6809{..} (JSR (Extend v)) =
   m6809{s=s-2, mem=poke2 mem (s-2) (pc+3), nextPC=Just v}
runOpcode m6809@M6809{..} (JSR (IndexAddr v)) =
   m6809{s=s-2, mem=poke2 mem (s-2) (pc+4), nextPC=Just $ peek2 mem v}
--runOpcode m6809@M6809{..} (JSR v) =
--   m6809{s=s-2, mem=poke2 mem (s-2) (pc+4), nextPC=Just $ peek2 mem $ getAddrFromValue m6809 v}

  
{-runOpcode m6809 (LDA (Immediate1 v)) = m6809{a=v}
runOpcode m6809@M6809{..} (LDA (Direct v)) = m6809{a=peek mem v}
runOpcode m6809@M6809{..} (LDA (Extend v)) = m6809{a=peek mem v} -}
{-runOpcode m6809@M6809{..} (LDA (IndexInc "X" v)) =
  m6809{a=a', x=x+fromIntegral v, cc=cc'}
  where
    a'=peek mem x
    cc'=if a' == 0 then set Z cc else clear Z cc --TODO add more stuff here
runOpcode m6809@M6809{..} (LDA (IndexInc "U" v)) =
  m6809{a=a', u=u+fromIntegral v, cc=cc'}
  where
    a'=peek mem u
    cc'=if a' == 0 then set Z cc else clear Z cc --TODO add more stuff here -}
runOpcode m6809@M6809{..} (LDA v) =
  step v $ m6809{a=a', cc=cc'}
  where
    a'=getWord8FromValue m6809 v
    cc'=if a' == 0 then set Z cc else clear Z cc --TODO add more stuff here
 
{-
runOpcode m6809 (LDB (Immediate1 v)) = m6809{b=v}
runOpcode m6809@M6809{..} (LDB (IndexInc "X" v)) =
  m6809{b=peek mem x, x=x+fromIntegral v}
runOpcode m6809@M6809{..} (LDB (IndexBase (NumBase v) "U")) =
  m6809{b=peek mem $ v + u}
runOpcode m6809@M6809{..} (LDB (IndexBase (NumBase v) "X")) =
  m6809{b=peek mem $ v + x}
runOpcode m6809@M6809{..} (LDB (IndexBase (NumBase v) "S")) =
  m6809{b=peek mem $ v + s}
runOpcode m6809@M6809{..} (LDB (Direct v)) = m6809{b=peek mem v}
-}

runOpcode m6809@M6809{..} (LDB v) =
  step v $ m6809{b=getWord8FromValue m6809 v}




runOpcode m6809 (LDD (Immediate2 v)) =
  m6809{a=b1, b=b2}
  where
    b1 = fromIntegral $ v `shiftR` 8
    b2 = fromIntegral v

runOpcode m6809@M6809{..} (LDD (Direct v)) =
  m6809{a=peek mem v, b=peek mem $ v+1}


runOpcode m6809@M6809{..} (LDD (IndexInc "X" v)) =
  m6809{x=x+ fromIntegral v, a=peek mem x, b=peek mem $ x+1}
runOpcode m6809@M6809{..} (LDD (IndexBase (NumBase v) "X")) =
  m6809{a=peek mem $ fromIntegral b+x, b=peek mem $ fromIntegral b+x+1}



runOpcode m6809@M6809{..} (LDS (Direct v)) = m6809{s=peek2 mem v}
runOpcode m6809 (LDS (Immediate2 v)) = m6809{s=v}

runOpcode m6809 (LDU (Immediate2 v)) = m6809{u=v}
runOpcode m6809@M6809{..} (LDU (Direct v)) = m6809{u=peek2 mem v}

runOpcode m6809 (LDU v) = m6809{u=getWord16FromValue m6809 v}

runOpcode m6809@M6809{..} (LDX (Direct v)) = m6809{x=peek2 mem v}
runOpcode m6809 (LDX (Immediate2 v)) = m6809{x=v}



runOpcode m6809@M6809{..} (LDX (IndexBase (NumBase v) "X")) =
  m6809{x=peek2 mem $ v+x}

runOpcode m6809@M6809{..} (LDX (IndexInc "S" v)) =
  m6809{s=s+fromIntegral v, x=peek2 mem s}
runOpcode m6809@M6809{..} (LDX (IndexBase (NumBase v) "S")) =
  m6809{x=peek2 mem $ v+s}
runOpcode m6809@M6809{..} (LDX (IndexBase (RegBase "B") "X")) =
  m6809{x=peek2 mem $ fromIntegral b+x}
runOpcode m6809@M6809{..} (LDX v) = step v $ m6809{x=getWord16FromValue m6809 v}




runOpcode m6809@M6809{..} (LEAS (IndexBase (NumBase base) "S")) =
  m6809{s=s', cc=cc'}
  where
    s'=s+base
    cc' = if s' == 0
          then set Z cc
          else clear Z cc

runOpcode m6809@M6809{..} (LEAX (IndexBase (NumBase base) "X")) =
  m6809{x=x', cc=cc'}
  where
    x'=x+base
    cc' = if x' == 0
          then set Z cc
          else clear Z cc


runOpcode m6809@M6809{..} LSLA =
  m6809{a=shiftL a 1, cc = if testBit a 7 then set C cc else clear C cc}
runOpcode m6809@M6809{..} LSLB =
  m6809{b=shiftL b 1, cc = if testBit b 7 then set C cc else clear C cc}
runOpcode m6809@M6809{..} LSRA =
  m6809{a=shiftR a 1, cc = if testBit a 0 then set C cc else clear C cc}
runOpcode m6809@M6809{..} LSRB =
  m6809{b=shiftR b 1, cc = if testBit b 0 then set C cc else clear C cc}


runOpcode m6809@M6809{..} (NEG v) =
  m6809{mem=poke mem addr (-(peek mem addr))}
  where
    addr = getAddrFromValue m6809 v


--runOpcode m6809@M6809{..} (ORA (Immediate1 v)) = m6809{a=a .|. v}
runOpcode m6809@M6809{..} (ORA v) = step v $ m6809{a=a .|. getWord8FromValue m6809 v}

--runOpcode m6809@M6809{..} (ORB (Immediate1 v)) = m6809{b=b .|. v}
runOpcode m6809@M6809{..} (ORB v) = step v $ m6809{b=b .|. getWord8FromValue m6809 v}

runOpcode m6809@M6809{..} (ORCC (Immediate1 v)) = m6809{cc=cc .|. v}

runOpcode m6809@M6809{..} (PSHS regs) =
  foldl (.) id (map pshs regs) m6809

runOpcode m6809@M6809{..} (PULS regs) =
  foldl (.) id (map puls regs) m6809

runOpcode m6809@M6809{..} (ROL (IndexBase (NumBase base) "U")) =
  m6809{mem=poke mem (base+u) v',cc=cc'}
  where
    v = peek mem (base+u)
    v' = v `shiftL` 1 + if test C cc then 1 else 0
    cc' = if testBit v 7 then set C cc else clear C cc

runOpcode m6809@M6809{..} ROLA =
  m6809{a=a',cc=cc'}
  where
    a' = a `shiftL` 1 + if test C cc then 1 else 0
    cc' = if testBit a 7 then set C cc else clear C cc


runOpcode m6809@M6809{..} (SBCA v) =
  step v $ m6809{a=a-getWord8FromValue m6809 v - (if test C cc then 1 else 0)}


runOpcode m6809@M6809{..} (STA v) =
  step v $ m6809{mem=poke mem (getAddrFromValue m6809 v) a}
  
runOpcode m6809@M6809{..} (STB v) =
  step v $ m6809{mem=poke mem (getAddrFromValue m6809 v) b}

runOpcode m6809@M6809{..} (STD v) =
  step v $ m6809{mem=poke (poke mem addr a) (addr+1) b}
  where
    addr = getAddrFromValue m6809 v

runOpcode m6809@M6809{..} (STX v) =
  step v $ m6809{mem=poke2 mem (getAddrFromValue m6809 v) x}

runOpcode m6809@M6809{..} (STU v) =
  step v $ m6809{mem=poke2 mem (getAddrFromValue m6809 v) u}





runOpcode m6809@M6809{..} (SUBA v) =
  step v $ m6809{a=a - getWord8FromValue m6809 v}



runOpcode m6809@M6809{..} (SUBD (IndexInc "S" v)) =
  m6809{s=s+fromIntegral v, a=a', b=b'}
  where
    d = fromIntegral a `shiftL` 8 + fromIntegral b
    d' = d - peek2 mem s
    a' = fromIntegral $ d' `shiftR` 8
    b' = fromIntegral d'
    
runOpcode m6809@M6809{..} RTS =
  m6809{s=s+2, nextPC=Just $ peek2 mem s}

runOpcode m6809@M6809{..} (TFR "A" "B") = m6809{b=a}
runOpcode m6809@M6809{..} (TFR "X" "S") = m6809{s=x}
runOpcode m6809@M6809{..} (TFR "X" "D") =
  m6809{a=fromIntegral $ x `shiftR` 8, b=fromIntegral $ x .&. 0xff}

runOpcode m6809@M6809{..} TSTA =
  m6809{cc=
        (if a==0 then set Z else clear Z)
        $ (if a > 127 then set N else clear N)
        $ clear V cc}

runOpcode m6809@M6809{..} TSTB =
  m6809{cc=
        (if b==0 then set Z else clear Z)
        $ (if b > 127 then set N else clear N)
        $ clear V cc}

runOpcode m6809@M6809{..} (TST (Direct p)) =
  m6809{cc=
        (if v==0 then set Z else clear Z)
        $ (if v > 127 then set N else clear N)
        $ clear V cc}
  where
    v = peek mem p

runOpcode m6809@M6809{..} (TST (IndexBase (NumBase p) "U")) =
  m6809{cc=
        (if v==0 then set Z else clear Z)
        $ (if v > 127 then set N else clear N)
        $ clear V cc}
  where
    v = peek mem $ p+u

runOpcode _ v = error $ "can't yet handle opcode: " ++ show v





pshs::Reg->M6809->M6809
pshs CCR m6809@M6809{..} = pshs8 cc m6809
pshs A m6809@M6809{..} = pshs8 a m6809
pshs B m6809@M6809{..} = pshs8 b m6809
pshs DPR m6809@M6809{..} = pshs8 undefined m6809
pshs X m6809@M6809{..} = pshs16 x m6809
pshs Y m6809@M6809{..} = pshs16 y m6809
pshs S m6809@M6809{..} = pshs16 s m6809
pshs U m6809@M6809{..} = pshs16 u m6809
pshs PC m6809@M6809{..} = pshs16 pc m6809

pshs8::Word8->M6809->M6809
pshs8 v m6809@M6809{..} = m6809{s=s-1, mem=poke mem (s-1) v}

pshs16::Word16->M6809->M6809
pshs16 v m6809@M6809{..} = m6809{s=s-2, mem=poke2 mem (s-2) v}

puls::Reg->M6809->M6809
puls CCR m6809@M6809{..} = m6809{s=s+1, cc=peek mem s}
puls A m6809@M6809{..} = m6809{s=s+1, a=peek mem s}
puls B m6809@M6809{..} = m6809{s=s+1, b=peek mem s}
puls DPR _ = undefined -- puls8 undefined m6809
puls X m6809@M6809{..} = m6809{s=s+2, x=peek2 mem s}
puls Y m6809@M6809{..} = m6809{s=s+2, y=peek2 mem s}
puls U m6809@M6809{..} = m6809{s=s+2, u=peek2 mem s}
puls PC m6809@M6809{..} = m6809{s=s+2, nextPC=Just $ peek2 mem s}
puls S _ = error "I don't think you are allowed to pull S in a puls"

