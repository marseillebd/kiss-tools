{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Char (chr, ord)
import Data.List ((!?))
import Data.Word (Word32)
import System.Exit (exitFailure)
import System.IO (stdout, hPutChar, hFlush, hSetBinaryMode)

-- * Goal
--
-- $goal
--
-- This is a quick-and-dirty reference implementation of a kiss-tools virtual machine.
-- The designed virtual machine should
--   be easily hand-written in machine code,
--   support an instruction format that is easy to write by-hand,
--   and perform simple interpretation and compilation tasks.
--
-- The design is not yet final, and I would like to experiment with working implementations.
-- I an as-yet unworried about minimizing build- and run-time dependencies, though I am tracking them (see 'Sys').
-- It is important that I choose tools to do this quickly, since I alone am responsible for the entire stack.
-- I am looking to build an implementation that will translate fairly well to C or a similar systems programming language.
--
-- Once the design is finalized, this implementation will serve as a way to test additional implementation.
-- Where another implementation differs from this one, one (likely the other) must be incorrect.

-- * Instruction Set Interface
--
-- $isa

data Vm = Vm
  { dataStack :: [Word32] -- ^ this stack holds operands
  , codeTable :: [Code] -- ^ an array of unevaluated code sections
  , controlStack :: [Code] -- ^ stack of code sections; the top is just the rest of the instructions in the current function; when the top is empty, it is popped thus returning control to the caller
  , memory :: [()] -- ^ TODO
  }

-- FIXME dynamic memory?
-- Forth's philosophy is really that everything is global.
-- "Thinking Forth" suggests preferring pure functions, therefore pass state on the stack.
-- However, what about dynamically-sized state, like an array of lines or tokens?
-- Or indeed, just the desire to pass a large value by (hopefully immutable) reference?
-- We're going to need a memory, or the "dump" of a SECD machine, but I don't want to implement gc or force a lot of memory management on the user.
--
-- I'm thinking about arenas. Arenas can grow (or shrink). References pair together an arena id with an index into the arena.
--
-- Perhaps something like this for a 32-bit addr space:
--
-- @
--  31      23 22          2 1           0
-- |----------|-------------|-------------|
-- | arena id | word offset | byte offset |
-- |----------|-------------|-------------|
--  9b = 512  * 21b = 2MiW  * 2b = 4B
-- @


type Code = [Instr]
data Instr
  = Imm Word32
  | Put
  | Add
  | Call Int -- NOTE that we can (should!) implement tail calls just by checking if the control stack is topped with an empty list, so we can implement aliases at least

class (Monad m) => Sys m where
  runSys :: m a -> IO a
  putByte :: Char -> m ()

-- * Machine Implementation
--
-- $impl

data Fault = StackUnderflow | CodeOverrun | Undefined
newtype M m a = M { unM :: Vm -> m (Either Fault a, Vm) }
instance (Monad m) => Functor (M m) where
  fmap f getX = M $ \vm0 ->
    unM getX vm0 >>= \case
      (Right x, vm1) -> pure (Right $ f x, vm1)
      (Left err, vm1) -> pure (Left err, vm1)

instance (Monad m) => Applicative (M m) where
  pure x = M $ \vm -> pure (Right x, vm)
  getF <*> getX = M $ \vm0 ->
    unM getF vm0 >>= \case
      (Right f, vm1) -> unM getX vm1 >>= \case
        (Right x, vm2) -> pure (Right $ f x, vm2)
        (Left err, vm2) -> pure (Left err, vm2)
      (Left err, vm1) -> pure (Left err, vm1)
instance (Monad m) => Monad (M m) where
  getX >>= k = M $ \vm0 -> do
    unM getX vm0 >>= \case
      (Right x, vm1) -> unM (k x) vm1
      (Left err, vm1) -> pure (Left err, vm1)

runM :: Vm -> M m a -> m (Either Fault a, Vm)
runM vm action = unM action vm

state :: (Monad m) => (Vm -> (a, Vm)) -> M m a
state action = M $ \vm0 -> pure (first Right $ action vm0)
fault :: (Monad m) => Fault -> M m a
fault err = M $ \vm -> pure (Left err, vm)
sys :: (Sys m) => m a -> M m a
sys getX = M $ \vm -> do
  x <- getX
  pure (Right x, vm)

step :: Sys m => M m ()
step = do
  op <- state fetch >>= \case
    Just op -> pure op
    Nothing -> fault CodeOverrun
  case op of
    Imm x -> push x
    Add -> do
      a <- pop
      b <- pop
      push (a + b)
    Put -> do
      c <- pop
      sys $ putByte (chr $ fromIntegral c)
    Call fptr -> do
      callee <- (state $ \vm -> (vm.codeTable !? fptr, vm)) >>= \case
        Just it -> pure it
        Nothing -> fault Undefined
      state $ \vm -> case vm.controlStack of
        -- what if there were multiple empty farmes on the control stack? there shouldn't be, because only `call` introduces the frames (one at a time)
        [] : callers -> ((), vm{controlStack = callee : callers})
        callers -> ((), vm{controlStack = callee : callers})
  where
  fetch :: Vm -> (Maybe Instr, Vm)
  fetch vm = case vm.controlStack of
    (op : ops) : callers -> (Just op, vm{controlStack = ops : callers})
    [] : callers -> fetch vm{controlStack = callers}
    [] -> (Nothing, vm)
  push :: Monad m => Word32 -> M m ()
  push x = do
    state $ \vm -> ((), vm{dataStack = x : vm.dataStack})
  pop :: Monad m => M m Word32
  pop = do
    r <- state $ \vm -> case vm.dataStack of
      x : xs -> (Just x, vm{dataStack = xs})
      [] -> (Nothing, vm)
    maybe (fault StackUnderflow) pure r


--   exec :: Sys m => Vm -> Instr -> m Vm
--   exec = undefined

-- * Firmware
--
-- $firmware
--
-- The rest of the code implements the 'Sys' interface and starts up the machine ('main').
-- This has been collected at the end of the file because it is the most platform-specific,
--   and will need to be swapped out in essentially any port.

main :: IO ()
main = do
  let vm = Vm
        { dataStack = []
        , codeTable =
          [ [Add] -- "+" (a b -- a+b): 0
          ]
        , controlStack =
          [ [Imm 5, Imm 5, Call 0] ++ [Imm (fromIntegral $ ord c) | c <- reverse "Hallo" ] ++ replicate 6 Put
          ]
        , memory = []
        }
  (r, _) <- runSys @IO . runM vm $ forM_ [0..14] $ \_ -> step
  case r of
    Right () -> pure ()
    Left err -> exitFailure


instance Sys IO where
  runSys action = do
    hSetBinaryMode stdout True
    it <- action
    hFlush stdout
    pure it
  putByte = hPutChar stdout

-- * Notes On Porting
--
-- $lowlevel
--
-- There will come a time when this implementation gets ported to a (much) more limited platform.
-- I'm especially hoping for a port into "standalone", ie no OS.
-- 
-- ** Memory Management
--
-- Dynamic memory will be needed, but what should be saved as part of the image, and what discarded?
-- Let's examine some uses of memory segments in ordinary systems programming:
--
-- - Code: the code table, no problem.
-- - Constants: easily implemented as words that add the constant onto the stack, thus code table.
-- - Load-time/initialized data: eg detecting system capabilities at startup, use the variable table.
-- - Small temporaries: it is enough to store these on the data stack
-- - Large temporaries: these should not be saved, but they may need to grow dynamically, which means some sort of 
-- - Long-lasting, possibly large data: eg a symbol table for use in a compiler. Use the variable table to save them. Avoid using addresses.
--   My first thought is that they would be created as a large temporary, then the necessary data would be traced and saved into the variable table.
--   However, if the variable table would support growing, then it's no big deal.
--
-- Here's the dynamically-growing sections we'll need:
-- - code table: append-only, saved
-- - fault table: append-only
-- - variable table: append-only table of mutable buffers, each variable independently statically-declared (like code), growable, possibly saved
-- - data stack: could be fixed-size, maybe saved
-- - file/device table: could be fixed-size again
--
-- Of these, only the variables (not the table) gets their size driven by the user.
-- What I'm thinking is that we use mmap (posix) and mremap (linux, bsd)
--
-- - [Posix std for mmap](https://pubs.opengroup.org/onlinepubs/9799919799/functions/mmap.html)
-- - [Lunix man page for mremap](https://www.man7.org/linux/man-pages/man2/mremap.2.html)
-- - [FreeBSD man page for mremap](https://man.freebsd.org/cgi/man.cgi?query=mremap&manpath=NetBSD+9.3)
-- - remap can be implemented for posix if we are ok with copying
-- - [Windows equivalent of anonymous mmap](https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc)
--
-- What I'd love is some way to actually test if we have a feature (such as nremap) at compiletime.
-- This isn't supported by standard C.
-- Instead, you need to write a configure script.
-- You can use autotools, but you could write it by hand for small cases.
-- The script will probably have to try compiling some C and seeing if it works/breaks.
-- Based on the result, it can define pre-processor macros in a generated `config.h`, which you then rely on in your real code.
