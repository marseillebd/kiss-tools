#!/usr/bin/env runghc
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Prelude hiding (cycle)

import Control.Exception (catch, throw)
import Control.Monad (forM, forM_, when)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.Char (ord, chr)
import GHC.Exts (MutableByteArray#, newByteArray#, RealWorld, Int(I#), (*#))
import GHC.Exts (readWord32Array#, writeWord32Array#)
import GHC.Exts (readWord8Array#, writeWord8Array#)
import GHC.Internal.Word (Word8(W8#), Word32(W32#), Word16)
import GHC.IO (IO(IO))
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO.Error (IOError, isEOFError)
import System.IO (stdin, stdout, stderr, withFile, IOMode(ReadMode), hGetChar, hPutChar, hSetEncoding, latin1, hPutStrLn, Handle)

main :: IO ()
main = do
  progFile <- getArgs >>= \case
    "-?" : _ -> help >> exitWith ExitSuccess
    "--help" : _ -> help >> exitWith ExitSuccess
    ["--", path] -> pure path
    [path] -> pure path
    -- TODO accept "-" as stdin
    _ -> help >> exitWith (ExitFailure 1)
  vm <- withFile progFile ReadMode $ \fp -> do
    hSetEncoding fp latin1
    -- find header
    c0 <- hGetChar fp
    c1 <- case c0 of
      '#' -> loop
        where
        loop = hGetChar fp >>= \case
          '\n' -> hGetChar fp
          _ -> loop
      _ -> pure c0
    -- load header
    -- lead header: first 16 bytes
    magic <- fmap (c1:) $ forM [1..7] $ \_ -> hGetChar fp
    case magic of
      "kiss vm\0" -> pure ()
      _ -> error "missing magic"
    let be32 :: IO Word32
        be32 = do
          bytes <- forM [1..4] $ \_ -> fromIntegral . ord <$> hGetChar fp
          pure $ foldl (\hi lo -> (hi `shiftL` 8) .|. lo) 0 bytes
    forM_ [1..8] $ \_ -> hGetChar fp -- 8-byte padding
    entryPoint <- be32
    codeSize <- be32
    stackBase <- be32
    maxMem <- be32
    when (codeSize <= entryPoint) $ error "entry point exceeds program size"
    when (stackBase < codeSize) $ error "stack position starts inside program"
    when (maxMem < stackBase) $ error "stack position exceeds max memory"
    vm <- newVM maxMem
    -- ensure address 0 has contents 0
    st32 vm 0 0
    -- load end of program to address 4
    st32 vm 4 codeSize
    -- load the stack base into address 8
    st32 vm 8 stackBase
    -- load maximum memory into address 12
    st32 vm 12 maxMem
    -- load the rest of program text into memory, after maxMem
    forM_ [16..codeSize-1] $ \i -> do
      b <- fromIntegral . ord <$> hGetChar fp
      st8 vm i b
    -- initialize special registers
    writeReg vm 12 stackBase -- set fp
    writeReg vm 13 stackBase -- set sp
    writeReg vm 15 entryPoint -- set ip
    pure vm
  -- setup environment
  hSetEncoding stdin latin1
  hSetEncoding stdout latin1
  -- begin execution
  let loop = cycle vm >> loop
  loop

help :: IO ()
help = do
  putStrLn "usage: kissvm PROG"
  putStrLn "    loads and executes a kiss vm binary"
  putStrLn ""
  putStrLn "  --help, -?: display this help message and exit"

debug :: String -> IO ()
debug msg = pure () -- hPutStrLn stderr msg

data VM = VM
  { mem :: MutableByteArray# RealWorld
  , regs :: MutableByteArray# RealWorld
  }

cycle :: VM -> IO ()
cycle vm = do
  --- fetch ---
  ip <- readReg vm 15
  instr <- ld16 vm ip
  debug $ showHex ip $ ' ' : showHex instr ""
  writeReg vm 15 (ip + 2)
  --- decode ---
  let opcode = (instr `shiftR` 12) .&. 0xF
      dst = fromIntegral $ (instr `shiftR` 8) .&. 0xF
      src = fromIntegral $ instr .&. 0xF
      r = (instr .&. 0x80) /= 0
      func = (instr `shiftR` 4) .&. 0x7
      imm = fromIntegral $ instr .&. 0xFF
  --- execute ---
  let readRegIsrc = if r then readReg vm src else pure imm
      readRegI4src = if r then readReg vm src else pure (fromIntegral src)
      binary f = do
        f <$> readReg vm dst <*> readRegIsrc
  case opcode of
    -- arithmetic
    0 -> writeReg vm dst =<< binary (+)
    1 -> writeReg vm dst =<< binary (-)
    2 -> writeReg vm dst =<< binary (*)
    3 -> writeReg vm dst =<< binary div
    -- logic
    4 -> writeReg vm dst =<< binary (.&.)
    5 -> do
      a <- readReg vm dst
      b <- readRegIsrc
      debug $ "or: " ++ showHex a (" " ++ showHex b "")
      writeReg vm dst =<< binary (.|.)
    6 -> do
      a <- readReg vm dst
      b <- readRegIsrc
      debug $ "shl: " ++ showHex a (" " ++ showHex b "")
      writeReg vm dst =<< binary (\a b -> a `shiftL` fromIntegral b)
    7 -> writeReg vm dst =<< binary (\a b -> a `shiftR` fromIntegral b)
    -- memory
    8 -> writeReg vm dst =<< ld32 vm =<< readRegIsrc
    9 -> bind2 (st32 vm) (readReg vm dst) readRegIsrc
    10 -> writeReg vm dst =<< fmap fromIntegral . ld8 vm =<< readRegIsrc
    11 -> bind2 (st8 vm) (readReg vm dst) (fromIntegral <$> readRegIsrc)
    -- other
    12 -> do -- conditionals
      a <- readReg vm dst
      b <- readRegI4src
      let cond = if func == 0 then 8 else func
          flags = (if a == b then 1 else 0) .|.
                  (if a < b then 2 else 0) .|.
                  (if a > b then 4 else 0) .|.
                  (if a > 0x7FFF_FFFF then 8 else 0)
      debug $ show [a, b] ++ show [cond, flags]
      if cond .&. flags == 0
      then writeReg vm 15 (ip + 4) -- advance past this instr and the next one, making the advance in fetch moot
      else pure ()
    13 -> do -- load immediate
      v0 <- readReg vm dst
      let v1 = (v0 `shiftL` 8) .|. imm
      writeReg vm dst v1
    14 -> do -- move
      let off = 2 * fromIntegral func
      a <- if r then (off +) <$> readReg vm src else pure imm
      writeReg vm dst a
    15 -> -- system
      case func of
        0 -> do -- read
          a <- vmGet vm
          debug $ "in: " ++ (if a >= 256 then "EOF" else show . chr $ fromIntegral a) ++ " " ++ showHex a ""
          writeReg vm dst a
        1 -> do -- write
          vmPut vm =<< readReg vm dst
        7 -> do -- halt
          ec <- readRegI4src
          let hsEc = if ec == 0 then ExitSuccess else ExitFailure (fromIntegral ec)
          exitWith hsEc
        _ -> error $ "unknown system call number: " ++ show func ++ " at " ++ showHex ip ""
  pure ()


bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f aM bM = do
  a <- aM
  b <- bM
  f a b

---------------------------------------------------------
------ this stuff would be really easy in C or asm ------
---------------------------------------------------------

vmGet :: VM -> IO Word32
vmGet vm = do
  i <- (ord <$> hGetChar stdin) `catch` onEofError
  pure $ fromIntegral i
  where
  onEofError :: IOError -> IO Int
  onEofError exn = if isEOFError exn then pure (-1) else throw exn

vmPut :: VM -> Word32 -> IO ()
vmPut vm v = hPutChar stdout $ (chr . fromIntegral) v

newVM :: Word32 -> IO VM
newVM len = do
  let I# len# = fromIntegral len
  IO $ \st0# ->
    case newByteArray# len# st0# of
    (# st1#, arr #) -> case newByteArray# (4# *# 16#) st1# of
      (# st2#, regs #) -> (# st2#, VM arr regs #)

ld8 :: VM -> Word32 -> IO Word8
ld8 vm addr = let I# addr# = fromIntegral addr
  in IO $ \st0# ->
    case readWord8Array# vm.mem addr# st0# of
    (# st1, v# #) -> (# st1, W8# v# #)

st8 :: VM -> Word32 -> Word8 -> IO ()
st8 vm addr (W8# v#) = let I# addr# = fromIntegral addr
  in IO $ \st0# ->
    case writeWord8Array# vm.mem addr# v# st0# of
    st1# -> (# st1#, () #)

ld16 :: VM -> Word32 -> IO Word16
ld16 vm addr = do
  hi <- fromIntegral <$> ld8 vm addr
  lo <- fromIntegral <$> ld8 vm (addr + 1)
  pure $ (hi `shiftL` 8) .|. lo

ld32 :: VM -> Word32 -> IO Word32
ld32 vm addr = do
  bytes <- forM [0..3] $ \i -> fromIntegral <$> ld8 vm (addr + 1)
  pure $ foldr (\hi lo -> (hi `shiftL` 8) .|. lo) 0 bytes

st32 :: VM -> Word32 -> Word32 -> IO ()
st32 vm addr w32 = do
  forM_ [0..3] $ \off -> do
    let byteIx = 3 - fromIntegral off
        w8 = (w32 `shiftR` byteIx) .&. 0xFF
    st8 vm (addr + off) (fromIntegral w8)

readReg :: VM -> Int -> IO Word32
readReg vm (I# i#) = IO $ \st0# ->
  case readWord32Array# vm.regs i# st0# of
  (# st1#, v# #) -> (# st1#, W32# v# #)

writeReg :: VM -> Int -> Word32 -> IO ()
writeReg vm (I# i#) (W32# v#) = IO $ \st0# ->
  case writeWord32Array# vm.regs i# v# st0# of
  st1# -> (# st1#, () #)
