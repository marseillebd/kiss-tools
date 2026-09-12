{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Forth where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, evalStateT, get, gets, put, modify)
import Control.Monad (when, forever)
import Data.List ((!?))
import Data.Maybe (isJust)
import Data.String (IsString(..))
import System.Exit (exitSuccess, ExitCode(..), exitWith, die)
import Text.Read (readMaybe)

-- TODO: so we basically have a forth machine
-- TODO: now we need a forth compiler, at first just compiling to a Load
-- TODO: but really I want to target either x64 (b/c that's my native machine) or riscv (because plain riscv is a small and open ISA)
-- TODO: and maybe a forth interpreter

-- NOTE I'm sure I'll want to come up with a mangling scheme, since this could be an ABI.
-- Names matching the glob `uf__*` are reserved (no initial undescore bc gcc reserves it).
-- Otherwise, names are z-encoded and then prefixed with `uf_`.
-- The call convention is custom to the architecture, probably storing forth's instruction, stack, and ctrl pointers in registers, and maybe some others, with arguments on the (forth) stack.
-- This lets us write some assembly under the label `<name>`
--   to adapt the calling conventions from system to forth for any exported `uf_<name>` labels the compiler emits.

------ a smoke test exe ------

main :: IO ()
main = do
  putStrLn "Hello, Forth!"
  let runtime = emptyLoad
        { funs =
          [ ("exit", inj primExit)
          , ("call", inj primCall)
          ]
        }
  let progText = unlines
        [ ":fun _start 42 ' exit call ;"
        ]
  prog <- case compileHs runtime progText of
    Left msg -> die msg
    Right ok -> pure ok
  enterForth prog (forever step)

----------------------------------
------ the abstract machine ------
----------------------------------

data Machine = M
  { pc :: !ProgramCounter
  , dStack :: ![Value]
  , cStack :: ![ProgramCounter]
  , code :: ![(String, Thread)]
  , mem :: ![(String, Variable)]
  }

data Value
  = Data Integer
  | Ptr String
  | VPtr (String, Int) -- varname + offset
  deriving (Show)

type Variable = [Value]

data Thread
  = PT PrimThread
  | UT UserThread
data PrimThread = PrimThread { prim :: Forth () }
data UserThread = UserThread { instrs :: [Instr] }
data Instr
  = Subr String
  | Imm Value
  | Return -- FIXME I dunno why I couldn't GADT this one to make it not constructable in actual lists of Instrs
  -- NOTE I thought about including "skip over a function that is defined here",
  -- but I decided againt is because that's harder to inspect the assembly/machine code for.

------ execution ------

newtype Forth a = F { unF :: StateT Machine IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

type ProgramCounter = (UserThread, Int)
fetch :: Forth Instr
fetch = F $ do
  m <- get
  let (thd, off) = m.pc
  case thd.instrs !? off of
    Just op -> do
      put $ m{pc = (thd, off+1)}
      pure op
    Nothing -> pure Return

pushData :: Value -> Forth ()
pushData v = F $ do
  modify $ \m -> m{ dStack = v : m.dStack }
popData :: Forth Value
popData = F $ do
  m <- get
  case m.dStack of
    tos : rest -> do
      put m{ dStack = rest }
      pure tos
    [] -> error "stack underflow"

-- NOTE TOS is top-of-stack, NOS is next-on-stack
-- NOTE while I haven't nseed it, 3OS, 4OS would be 3rd-, 4th-, etc on-stack

callSubr :: UserThread -> Forth ()
callSubr thd = F $ modify $ \m -> m
  { pc = (thd, 0)
  , cStack = m.pc : m.cStack
  }
retSubr :: Forth ()
retSubr = F $ modify $ \m -> case m.cStack of
  k : ctrl -> m
    { pc = k
    , cStack = ctrl
    }
  [] -> error "control stack undeflow"

step :: Forth ()
step = fetch >>= \case
  Subr name -> do
    m <- F get
    case lookup name m.code of
      Just (UT subr) -> callSubr subr
      Just (PT subr) -> subr.prim
      Nothing -> error $ "no such subroutine: " <> show name
  Imm v -> pushData v
  Return -> retSubr

------------------------
------ primitives ------
------------------------

-- NOTE if the machine were implemented in assembly, these would also be,
-- though perhaps as a library that can be linked with the compiler output

------ stack manipulation ------

-- TODO: npeek -- look n entries down (1-indexed) and copy it to tos
-- TODO: ndrop -- look n entries down and remove it from the stack
-- TODO: nrot -- look n entries down (1-indexed) and rotate it to tos
--            -- ie peekn(i) dropn(i+1)
-- TODO: nmove -- pop tos and replace n entries down (from now) with the result
-- TODO: ndup -- nmove, but don't drop tos
--            -- ie 0 ndup === dup
-- TODO: npush -- pop tos and insert it so that it is n entries down, moving everything else up

-- TODO: n{peek,drop,&c}.c -- same stuff but act on the control stack
-- TODO: n{peek,pop}.cd -- same stuff but move values from control to data
-- TODO: n{peek,pop}.dc -- same stuff but move values from data to control
-- TODO: or the motion ones could be save, restore, look, idk

------ control flow ------

-- TODO: if, probably while and maybe some other basic control flow
-- (when/unless, fold, do-while, idk, perhaps somehow switch or cond)

primCall :: Forth ()
primCall = do
  name <- popData >>= \case
    Ptr name -> pure name
    other -> error $ "type error: expected function pointer, got " <> show other
  defs <- F $ gets (.code)
  case lookup name defs of
    Just (UT it) -> callSubr it
    Just (PT it) -> it.prim
    Nothing -> error $ "no such function: " <> show name

------ arithmetic and logic ------

-- TODO: add, sub, {i,u}mul, {i,u}{div,mod,divmod}, neg
-- TODO: shl, {i,u}shr
-- TODO: and, or, xor, not
-- TODO: cmp, {n,}eq, {i,u}{l,g}{t,e}
-- TODO: something about wide/carry/borrow arithmetic

------ environment ------

primExit :: Forth ()
primExit = popData >>= \case
  Data 0 -> liftIO exitSuccess
  Data err -> liftIO $ exitWith (ExitFailure $ fromInteger err)
  other -> error $ "type error in exit: expecting int, got " <> show other

---------------------------------
------ Haskell Integration ------
---------------------------------

data Load = Load
  { entry :: !String
  , funs :: ![(String, Thread)]
  , arrs :: ![(String, Variable)]
  -- TODO we might have some posix? stuff: program name, argc, argc, stdin, stdout, env
  }

emptyLoad :: Load
emptyLoad = Load
  { entry = "_start"
  , funs = []
  , arrs = []
  }

enterForth :: Load -> Forth () -> IO a
enterForth env action = do
  thd0 <- case lookup env.entry env.funs of
    Just (UT thd0) -> pure thd0
    _ -> error "entry point must be user thread"
  evalStateT action.unF (m0 thd0)
  exitSuccess
  where
  m0 thd0 =  M
    { pc = (thd0, 0)
    , dStack = []
    , cStack = []
    , code = env.funs
    , mem = env.arrs
    }

----------------------------------
------ Compiler (to `Load`) ------
----------------------------------

compileHs :: Load -> String -> Either String Load
compileHs acc0 input = do
  -- FIXME allow string constants
  let processed = words . unlines $ fmap dropComment $ lines input
  loop acc0 processed
  where
  loop acc (":fun" : ws) = case span (/= ";") ws of
    (name : bodyStrs, (";" : rest)) -> do
      -- TODO probably check for forward declarations
      checkName acc name
      body <- loopFun [] bodyStrs
      loop acc
        { funs = (name, inj @Thread body) : acc.funs
        } rest
    ([], (";":_)) -> Left "missing function name"
    (_, _) -> Left "unterminated function definition"
  loop acc (":var" : rest) = loopVar acc rest
  loop acc (":arr" : rest) = loopArr acc rest
  loop _ (w : _) = Left $ "not a special form: " <> show w
  loop acc [] = pure acc

  loopFun _ ["\'"] = do
    Left $ "missing name after tick"
  loopFun acc ("\'":name:rest) = do
    when (isJust (readMaybe @Integer name)) $ do
      Left $ "expected name after tick, got: " <> show name
    let v = Imm $ Ptr name
    loopFun (v : acc) rest
  -- TODO check for bracketed code (which just means grab a gensym and splat the gensym name in)
  loopFun acc (w:rest) = do
    loopFun (fromString @Instr w : acc) rest
  loopFun acc [] = pure $ reverse acc
  loopArr = undefined
  loopVar = undefined

  checkName :: Load -> String -> Either String ()
  checkName acc name = do
    case lookup name acc.funs of
      Just _ -> Left $ "already defined: " <> show name
      _ -> pure ()
    case lookup name acc.arrs of
      Just _ -> Left $ "already defined: " <> show name
      _ -> pure ()
  dropComment = takeWhile (/= '#')


----------------------------
------ More Compilers ------
----------------------------

-- TODO target x64 assembly
-- TODO target x64 ELF hexdump or just plain binary
-- TODO target x32 BIOS, or maybe 8086, or even 8080 if I can find what the bootloader is

-- TODO target the minimal RiscV (assembly)
-- TODO target the minimal RiscV hexdump/binary (if I can find "the" bootloader)

---------------------
------ Helpers ------
---------------------

instance IsString Instr where
  fromString str = case readMaybe @Integer str of
    Just i -> Imm $ Data i
    Nothing -> Subr str -- TODO check constraints on subroutine names
instance IsString Thread where
  fromString str = UT . UserThread $ fromString <$> words str

class Inj to from where inj :: from -> to

instance Inj PrimThread (Forth ()) where inj = PrimThread
instance Inj Thread PrimThread where inj = PT
instance Inj Thread (Forth ()) where inj = inj . inj @PrimThread
instance Inj UserThread [Instr] where inj = UserThread
instance Inj Thread UserThread where inj = UT
instance Inj Thread [Instr] where inj = inj . inj @UserThread
