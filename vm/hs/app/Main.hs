{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Prelude hiding (cycle, div, and)
import qualified Prelude

import Control.Exception (catch, throw)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Bits (Bits, shiftR, shiftL, (.&.), (.|.), complement)
import Data.Char (ord, chr)
import Data.Primitive.ByteArray (MutableByteArray, newByteArray, readByteArray, writeByteArray)
import Data.Primitive (sizeOfType)
import Data.Word (Word8, Word16, Word32)
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Error (IOError, isEOFError)
import System.IO (stdin, stdout, stderr, withFile, IOMode(ReadMode), hGetChar, hPutChar, hSetEncoding, latin1, hPutStrLn, Handle)


main :: IO ()
main = do
  let instr = decode 0xF07A
  runVm $ do
    instr

  -- progFile <- getArgs >>= \case
  --   "-?" : _ -> help >> exitWith ExitSuccess
  --   "--help" : _ -> help >> exitWith ExitSuccess
  --   ["--", path] -> pure path
  --   [path] -> pure path
  --   -- TODO accept "-" as stdin
  --   _ -> help >> exitWith (ExitFailure 1)
  -- vm <- withFile progFile ReadMode $ \fp -> do
  --   hSetEncoding fp latin1
  --   -- find header
  --   c0 <- hGetChar fp
  --   c1 <- case c0 of
  --     '#' -> loop
  --       where
  --       loop = hGetChar fp >>= \case
  --         '\n' -> hGetChar fp
  --         _ -> loop
  --     _ -> pure c0
  --   -- load header
  --   -- lead header: first 16 bytes
  --   magic <- fmap (c1:) $ forM [1..7] $ \_ -> hGetChar fp
  --   case magic of
  --     "kiss vm\0" -> pure ()
  --     _ -> error "missing magic"
  --   let be32 :: IO Word32
  --       be32 = do
  --         bytes <- forM [1..4] $ \_ -> fromIntegral . ord <$> hGetChar fp
  --         pure $ foldl (\hi lo -> (hi `shiftL` 8) .|. lo) 0 bytes
  --   forM_ [1..8] $ \_ -> hGetChar fp -- 8-byte padding
  --   entryPoint <- be32
  --   codeSize <- be32
  --   stackBase <- be32
  --   maxMem <- be32
  --   when (codeSize <= entryPoint) $ error "entry point exceeds program size"
  --   when (stackBase < codeSize) $ error "stack position starts inside program"
  --   when (maxMem < stackBase) $ error "stack position exceeds max memory"
  --   vm <- newVM maxMem
  --   -- ensure address 0 has contents 0
  --   st32 vm 0 0
  --   -- load end of program to address 4
  --   st32 vm 4 codeSize
  --   -- load the stack base into address 8
  --   st32 vm 8 stackBase
  --   -- load maximum memory into address 12
  --   st32 vm 12 maxMem
  --   -- load the rest of program text into memory, after maxMem
  --   forM_ [16..codeSize-1] $ \i -> do
  --     b <- fromIntegral . ord <$> hGetChar fp
  --     st8 vm i b
  --   -- initialize special registers
  --   writeReg vm 12 stackBase -- set fp
  --   writeReg vm 13 stackBase -- set sp
  --   writeReg vm 15 entryPoint -- set ip
  --   pure vm
  -- -- setup environment
  -- hSetEncoding stdin latin1
  -- hSetEncoding stdout latin1
  -- -- begin execution
  -- let loop = cycle vm >> loop
  -- loop

help :: IO ()
help = do
  putStrLn "usage: kissvm PROG"
  putStrLn "    loads and executes a kiss vm binary"
  putStrLn ""
  putStrLn "  --help, -?: display this help message and exit"

debug :: String -> IO ()
debug msg = pure () -- hPutStrLn stderr msg

-- cycle :: VM -> IO ()
-- cycle vm = do
--   --- fetch ---
--   ip <- readReg vm 15
--   instr <- ld16 vm ip
--   debug $ showHex ip $ ' ' : showHex instr ""
--   writeReg vm 15 (ip + 2)
--   --- decode ---
--   let opcode = (instr `shiftR` 12) .&. 0xF
--       dst = fromIntegral $ (instr `shiftR` 8) .&. 0xF
--       src = fromIntegral $ instr .&. 0xF
--       r = (instr .&. 0x80) /= 0
--       func = (instr `shiftR` 4) .&. 0x7
--       imm = fromIntegral $ instr .&. 0xFF
--   --- execute ---
--   let readRegIsrc = if r then readReg vm src else pure imm
--       readRegI4src = if r then readReg vm src else pure (fromIntegral src)
--       binary f = do
--         f <$> readReg vm dst <*> readRegIsrc
--   case opcode of
--     -- arithmetic
--     0 -> writeReg vm dst =<< binary (+)
--     1 -> writeReg vm dst =<< binary (-)
--     2 -> writeReg vm dst =<< binary (*)
--     3 -> writeReg vm dst =<< binary Prelude.div
--     -- logic
--     4 -> writeReg vm dst =<< binary (.&.)
--     5 -> do
--       a <- readReg vm dst
--       b <- readRegIsrc
--       debug $ "or: " ++ showHex a (" " ++ showHex b "")
--       writeReg vm dst =<< binary (.|.)
--     6 -> do
--       a <- readReg vm dst
--       b <- readRegIsrc
--       debug $ "shl: " ++ showHex a (" " ++ showHex b "")
--       writeReg vm dst =<< binary (\a b -> a `shiftL` fromIntegral b)
--     7 -> writeReg vm dst =<< binary (\a b -> a `shiftR` fromIntegral b)
--     -- memory
--     8 -> writeReg vm dst =<< ld32 vm =<< readRegIsrc
--     9 -> bind2 (st32 vm) (readReg vm dst) readRegIsrc
--     10 -> writeReg vm dst =<< fmap fromIntegral . ld8 vm =<< readRegIsrc
--     11 -> bind2 (st8 vm) (readReg vm dst) (fromIntegral <$> readRegIsrc)
--     -- other
--     12 -> do -- conditionals
--       a <- readReg vm dst
--       b <- readRegI4src
--       let cond = if func == 0 then 8 else func
--           flags = (if a == b then 1 else 0) .|.
--                   (if a < b then 2 else 0) .|.
--                   (if a > b then 4 else 0) .|.
--                   (if a > 0x7FFF_FFFF then 8 else 0)
--       debug $ show [a, b] ++ show [cond, flags]
--       if cond .&. flags == 0
--       then writeReg vm 15 (ip + 4) -- advance past this instr and the next one, making the advance in fetch moot
--       else pure ()
--     13 -> do -- load immediate
--       v0 <- readReg vm dst
--       let v1 = (v0 `shiftL` 8) .|. imm
--       writeReg vm dst v1
--     14 -> do -- move
--       let off = 2 * fromIntegral func
--       a <- if r then (off +) <$> readReg vm src else pure imm
--       writeReg vm dst a
--     15 -> -- system
--       case func of
--         0 -> do -- read
--           a <- vmGet vm
--           debug $ "in: " ++ (if a >= 256 then "EOF" else show . chr $ fromIntegral a) ++ " " ++ showHex a ""
--           writeReg vm dst a
--         1 -> do -- write
--           vmPut vm =<< readReg vm dst
--         7 -> do -- halt
--           ec <- readRegI4src
--           let hsEc = if ec == 0 then ExitSuccess else ExitFailure (fromIntegral ec)
--           exitWith hsEc
--         _ -> error $ "unknown system call number: " ++ show func ++ " at " ++ showHex ip ""
--   pure ()


{-
Load instr can now be replaced by ordinary rs-type mov.

I could use the first 128 words for a symbol table. `mov ip [imm7]`

Yes, words are specified to be big-endian. That way you can write constants into the text without having to byteswap your hex arithmetic.


|          |                      |                                            |
| -------- | -------------------- | ------------------------------------------ |
| r0-r4    | caller save          |                                            |
| r5-r9    | callee save          |                                            |
| ra, rb   | scratch              |                                            |
| rc/ip/pc | caller save          | program counter                            |
| rd/sp    | callee restore       | stack pointer, aka dump                    |
| re/ep    | caller save, if used | environment pointer (closure, `this`, etc) |
| rf/fo    | caller save, if used | frame pointer                              |
|          |                      |                                            |

I don't wanna keep hand-assembling these hextools as the ISA changes.
So instead, I will program it in this new Haskell TTF API and use an evaluator to assemble and link the thing (I suppose this might be a good time to figure out recursive monads? or I could brute-force it by adding another TTF for noting and calcuating with addresses). (This program will still need to pad because) I want to then hand-check the output bytes, really just hand-disassembling and maybe even decompiling them. The output from the Haskell impl can still be a committed artifact "as if" it was hand-written. However, I want to attach "provenence" to each artifact. This one would be "generated by prototype assembler, then disassebled/decompiled and verified by hand". Provenence really end up being the T-diagrams, I suppose.

Well, here's some charts:
```
op mnem fmt
0  add  rs
1  sub  rs
2  mul  rs
3  div  rs
4  and  rs
5  or   rs  (nor?? bc that makes inv easy?)
6  shl  rs
7  shr  rs
8  mov  rs  (aka ld when addr mode is memory)
9  sto  rs
A  jal  rs  (R, pc <- pc, S)
B  
C  cCC  rfx
D  ldi  imm
E  
F  sys  rfx
...

Formats
rs:  ORMS
rfs: ORrfS
imm: ORI

Mode
0-7  : imm
8  9  A  B  C  D  E  F
r  ib li hi ii so eo fo
where:
imm7 = M.S                        small immediate
r    = regs[S]                    register direct
ib   = &mem8[regs[S]]             indirect byte
li   = &mem32[regs[S] & 0xffff]   low register indirect (car)
hi   = &mem32[regs[S] >> 16]      high register indirect (cdr)
ii   = &mem32[regs[S] + r4]       indexed indirect
so   = &mem32[sp - regs[S]]       stack offset
eo   = &mem32[ep + regs[S]]       environment offset
fo   = &mem32[fp + regs[S]]       frame offset


Condition Addressing Mode and Functions `rf`
      ilt | eq | lt | le | gt | ge | ne | 
imm4:  0  | 1  | 2  | 3  | 4  | 5  | 6  | 7
reg:   8  | 9  | A  | B  | C  | D  | E  | F
( so 3-input nor on f switches to subtract and test high bit,
  wheras all others are mask f with [.0 = eq,lt,gt] tests and succeed if non-zero
)

System Functions:
     get | put | | | | | | hlt
imm: 0   | 1   | | | | | | 7
reg: 8   | 9   | | | | | | F

Equivalencies
j +x
j -x
ldb d r4+a    ===    mov r (ib) a  `8dDa`
stb s r4+a    ===    sto s (ib) a  `9sDa`
neg d a       ===    mov d 0; sub d a
inv d a       ===    mov d 0; sub d 1; sub d a
inv d         ===    nor d d
or d s        ===    nor d s; nor d d
```

-}

--------------------
------ Decode ------
--------------------

decode :: Instruction r => Word16 -> r
decode instr =
  let opcode = (instr `shiftR` 12) .&. 0xF
      dst = OpdR . fromIntegral $ (instr `shiftR` 8) .&. 0xF
      opdS = decodeOpdS . fromIntegral $ instr .&. 0xFF
      opdX = decodeOpdX . fromIntegral $ instr .&. 0x8F
      cond = decodeCond . fromIntegral $ (instr `shiftR` 4) .&. 0x7
      sysfunc = decodeSysfunc . fromIntegral $ (instr `shiftR` 4) .&. 0x7
   in case opcode of
    0x0 -> add dst opdS
    0x1 -> sub dst opdS
    0x2 -> mul dst opdS
    0x3 -> div dst opdS
    0x4 -> and dst opdS
    0x5 -> nor dst opdS
    0x6 -> shl dst opdS
    0x7 -> shr dst opdS
    0x8 -> mov dst opdS
    0x9 -> sto dst opdS
    0xA -> jal dst opdS
    0xC -> cCC dst cond opdX
    0xD -> ldi dst (fromIntegral $ instr .&. 0xFF)
    0xF -> sys dst sysfunc opdX
    _ -> error "illegal opcode"

decodeOpdS :: Word8 -> OperandS
decodeOpdS x =
  let imm7 = fromIntegral $ x .&. 0x7F
      mode = case (x `shiftR` 4) .&. 0x7 of
        0 -> Direct
        1 -> IndexedByte
        2 -> LoIndirect
        3 -> HiIndirect
        4 -> IndexedIndirect
        5 -> Stack
        6 -> Env
        7 -> Frame
        _ -> error "unknown addressing mode"
   in if x .&. 0x80 == 0
      then OpdSImm imm7
      else OpdSReg mode (fromIntegral $ x .&. 0xF)

decodeOpdX :: Word8 -> OperandX
decodeOpdX x =
  let imm4 = fromIntegral $ x .&. 0x0F
   in if x .&. 0x80 == 0
      then OpdXImm imm4
      else OpdXReg (fromIntegral $ x .&. 0xF)

decodeCond :: Word4 -> Condition
decodeCond 0 = ILtZ
decodeCond 1 = UEq
decodeCond 2 = ULt
decodeCond 3 = ULe
decodeCond 4 = UGt
decodeCond 5 = UGe
decodeCond 6 = UNe
decodeCond _ = error "unknown condition"

decodeSysfunc :: Word4 -> SysFunc
decodeSysfunc 0 = Get
decodeSysfunc 1 = Put
decodeSysfunc 7 = Hlt
decodeSysfunc _ = error "unknown system code"

---------------------------------
------ Typed Tagless Final ------
---------------------------------

class Instruction r where
  add :: OperandR -> OperandS -> r
  sub :: OperandR -> OperandS -> r
  mul :: OperandR -> OperandS -> r
  div :: OperandR -> OperandS -> r
  and :: OperandR -> OperandS -> r
  nor :: OperandR -> OperandS -> r
  shl :: OperandR -> OperandS -> r
  shr :: OperandR -> OperandS -> r
  mov :: OperandR -> OperandS -> r
  sto :: OperandR -> OperandS -> r
  jal :: OperandR -> OperandS -> r
  -- idk
  cCC :: OperandR -> Condition -> OperandX -> r
  ldi :: OperandR -> Word8 -> r
  -- idk
  sys :: OperandR -> SysFunc -> OperandX -> r

data OperandR = OpdR Word4
data OperandS
  = OpdSImm Word7
  | OpdSReg AddrMode Word4
data OperandX
  = OpdXImm Word4
  | OpdXReg Word4

data AddrMode
  = Direct
  | IndexedByte
  | LoIndirect -- [s & 0xffff] ie car
  | HiIndirect -- [s >> 16] ie cdr
  | IndexedIndirect -- [s*4 + r4], cuz ig r4 is the index register, hoping for this mode to be encoded 0xC.
  | Stack -- [sp - s*4] 0xD for dump
  | Env -- [ep + s*4] 0xE
  | Frame -- [fp + s*4] 0xF

data Condition
  = ILtZ   -- FIXME not ILt?
  | UEq | UNe
  | ULt | ULe | UGt | UGe

data SysFunc
  = Get | Put
  | Hlt

------ Architectural Data Types ------

newtype Word4 = W4 { unW4 :: Word8 }
  deriving (Eq, Ord, Real)
instance Enum Word4 where
  fromEnum = fromEnum . unW4
  toEnum = W4 . (.&. 0xF) . fromIntegral
instance Num Word4 where
  fromInteger = W4 . (.&. 0xF) . fromInteger
  a + b = W4 . (0xF .&.) $ unW4 a + unW4 b
  a * b = W4 . (0xF .&.) $ unW4 a * unW4 b
  abs a = W4 . (0xF .&.) $ abs (unW4 a)
  signum a = W4 . (0xF .&.) $ signum (unW4 a)
  negate a = W4 . (0xF .&.) $ negate (unW4 a)
instance Integral Word4 where
  toInteger = toInteger . unW4
  a `quotRem` b = both (W4 . (.&. 0xF)) $ unW4 a `quotRem` unW4 b

newtype Word7 = W7 { unW7 :: Word8 }
  deriving (Eq, Ord, Real)
instance Enum Word7 where
  fromEnum = fromEnum . unW7
  toEnum = W7 . (.&. 0x7F) . fromIntegral
instance Num Word7 where
  fromInteger = W7 . (.&. 0x7F) . fromInteger
  a + b = W7 . (0x7F .&.) $ unW7 a + unW7 b
  a * b = W7 . (0x7F .&.) $ unW7 a * unW7 b
  abs a = W7 . (0x7F .&.) $ abs (unW7 a)
  signum a = W7 . (0x7F .&.) $ signum (unW7 a)
  negate a = W7 . (0x7F .&.) $ negate (unW7 a)
instance Integral Word7 where
  toInteger = toInteger . unW7
  a `quotRem` b = both (W7 . (.&. 0x7F)) $ unW7 a `quotRem` unW7 b

-----------------------------
------ Virtual Machine ------
-----------------------------

instance Instruction (Vm ()) where
  add dst src = binOpR (+) dst (effR dst) (effS src)
  sub dst src = binOpR (-) dst (effR dst) (effS src)
  mul dst src = binOpR (*) dst (effR dst) (effS src)
  div dst src = binOpR Prelude.div dst (effR dst) (effS src)
  and dst src = binOpR (.&.) dst (effR dst) (effS src)
  nor dst src = binOpR (.~|.) dst (effR dst) (effS src)
  shl dst src = binOpR shiftL dst (effR dst) (fromIntegral <$> effS src)
  shr dst src = binOpR shiftR dst (effR dst) (fromIntegral <$> effS src)
  mov dst src = binOpR const dst (effS src) (pure ())
  sto val addr = lea addr >>= \case
    Left byteAddr -> st8 byteAddr =<< (fromIntegral <$> effR val)
    Right wordAddr -> st32be wordAddr =<< ((4*) <$> effR val)
  jal (OpdR dst) src = do
    target <- effS src
    readReg 0xF >>= writeReg dst
    writeReg 0xF target
  cCC (OpdR a) f b = do
    x <- readReg a
    y <- effX b
    if condition x f y
    then pure ()
    else incPc
  ldi (OpdR dst) imm = writeReg dst (fromIntegral imm)
  sys (OpdR dst) Get _ = do
    writeReg dst =<< vmGet
  sys src Put _ = do
    effR src >>= vmPut
  sys _ Hlt src = do
    code <- fromIntegral . (.&. 0xFF) <$> effX src
    vmHalt code

binOpR :: (a -> b -> Word32)
      -> OperandR
      -> Vm a
      -> Vm b
      -> Vm ()
binOpR f (OpdR dst) a b = writeReg dst =<< (f <$> a <*> b)

effR :: OperandR -> Vm Word32
effR (OpdR i) = readReg i

effS :: OperandS -> Vm Word32
effS (OpdSImm w7) = pure $ fromIntegral w7
effS (OpdSReg mode i) = case mode of
  Direct -> readReg i
  IndexedByte -> fromIntegral <$> (readReg i >>= ld8)
  LoIndirect -> ld32be =<< (.&. 0xFFFF) <$> readReg i
  -- TODO

effX :: OperandX -> Vm Word32
effX (OpdXImm w4) = pure $ fromIntegral w4
effX (OpdXReg i) = readReg i

-- gets either a byte address or a word address
lea :: OperandS -> Vm (Either Word32 Word32)
lea (OpdSImm w7) = pure . Right $ fromIntegral w7
lea (OpdSReg mode i) = case mode of
  Direct -> Right <$> readReg i
  IndexedByte -> Left <$> readReg i

condition :: Word32 -> Condition -> Word32 -> Bool
condition a f b =
  let flags = (if a == b then 1 else 0) .|.
              (if a < b then 2 else 0) .|.
              (if a > b then 4 else 0) .|.
              (if a > 0x7FFF_FFFF then 8 else 0)
   in flags .&. mask f /= 0
  where
  mask :: Condition -> Int
  mask UEq = 1
  mask ULt = 2
  mask ULe = 3
  mask UGt = 4
  mask UGe = 5
  mask UNe = 6
  mask ILtZ = 8

------ The Machine ------
newtype Vm a = Vm { unVm :: ReaderT VmSt IO a }
  deriving (Functor, Applicative, Monad)
runVm :: Vm a -> IO a
runVm action = do
  st <- VmSt
    <$> newByteArray (fromIntegral $ 65536 * sizeOfType @Word32)
    <*> newByteArray (fromIntegral $ 16 * sizeOfType @Word32)
  runReaderT (unVm action) st

data VmSt = VmSt
  { mem :: MutableByteArray RealWorld
  , regs :: MutableByteArray RealWorld
  }

------ register file ------
readReg :: Word4 -> Vm Word32
readReg (fromIntegral -> i) = Vm $ ask >>= \vm -> readByteArray vm.regs i

writeReg :: Word4 -> Word32 -> Vm ()
writeReg (fromIntegral -> i) v = Vm $ ask >>= \vm -> writeByteArray vm.regs i v

incPc :: Vm ()
incPc = do
  ip0 <- readReg 0xF
  writeReg 0xF $ ip0 + 2

------ memory unit ------
ld8 :: Word32 -> Vm Word8
ld8 (fromIntegral -> addr) = Vm $ ask >>= \vm -> readByteArray vm.mem addr

st8 :: Word32 -> Word8 -> Vm ()
st8 (fromIntegral -> addr) v = Vm $ ask >>= \vm -> writeByteArray vm.mem addr v

ld16be :: Word32 -> Vm Word16
ld16be addr = do
  hi <- fromIntegral <$> ld8 addr
  lo <- fromIntegral <$> ld8 (addr + 1)
  pure $ (hi `shiftL` 8) .|. lo

ld32be :: Word32 -> Vm Word32
ld32be addr = do
  bytes <- forM [0..3] $ \i -> fromIntegral <$> ld8 (addr + i)
  pure $ foldr (\hi lo -> (hi `shiftL` 8) .|. lo) 0 bytes

st32be :: Word32 -> Word32 -> Vm ()
st32be addr w32 = do
  forM_ [0..3] $ \off -> do
    let byteIx = 3 - fromIntegral off
        w8 = fromIntegral $ (w32 `shiftR` byteIx) .&. 0xFF
    st8 (addr + off) w8

------ input/output ------
--FIXME these should be injected dependencies to the Vm monad

vmGet :: Vm Word32
vmGet = Vm $ do
  i <- liftIO $ (ord <$> hGetChar stdin) `catch` onEofError
  pure $ fromIntegral i
  where
  onEofError :: IOError -> IO Int
  onEofError exn = if isEOFError exn then pure (-1) else throw exn

vmPut :: Word32 -> Vm ()
vmPut v = Vm $ liftIO $ hPutChar stdout $ (chr . fromIntegral) v

vmHalt :: Word8 -> Vm ()
vmHalt 0 = Vm $ liftIO $ exitWith ExitSuccess
vmHalt code = Vm $ liftIO $ exitWith $ ExitFailure (fromIntegral code)

-----------------------
------ Assembler ------
-----------------------

-- instance Instruction r where
  -- add :: OperandR -> OperandS -> r
  -- sub :: OperandR -> OperandS -> r
  -- mul :: OperandR -> OperandS -> r
  -- div :: OperandR -> OperandS -> r
  -- and :: OperandR -> OperandS -> r
  -- nor :: OperandR -> OperandS -> r
  -- shl :: OperandR -> OperandS -> r
  -- shr :: OperandR -> OperandS -> r
  -- mov :: OperandR -> OperandS -> r
  -- sto :: OperandR -> OperandS -> r
  -- jal :: OperandR -> OperandS -> r
  -- -- idk
  -- cc :: OperandR -> Condition -> OperandX -> r
  -- ldi :: OperandR -> Word8 -> r
  -- -- idk
  -- sys :: OperandR -> SysFunc -> OperandX -> r

-- TODO for now, I'm just doing explicit two passes, but this could be done in a MonadFix I think

-- newtype Asm a = Asm { unAsm :: Map String Word16 -> Either (AsmError, (Map String Word16, a)) }

---------------------
------ Helpers ------
---------------------

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | Bitwise nor.
infixl 5 .~|.
(.~|.) :: Bits a => a -> a -> a
a .~|. b = complement (a .|. b)
