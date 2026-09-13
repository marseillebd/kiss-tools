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
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad (when, forever)
import Data.List ((!?), uncons)
import Data.Maybe (isJust, fromMaybe)
import Data.String (IsString(..))
import System.Exit (exitSuccess, ExitCode(..), exitWith, die)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS

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

-- NOTE TOS is top-of-stack, NOS is next-on-stack
-- NOTE while I haven't nseed it, 3OS, 4OS would be 3rd-, 4th-, etc on-stack

------ a smoke test exe ------

main :: IO ()
main = do
  putStrLn "Hello, Forth!"
  let runtime = emptyLoad
        { funs =
          [ ("exit", inj primExit)
          , ("put.b", inj primPutStdout)

          , ("ifnz", inj primIfnz)
          , ("jump", inj primJump)
          , ("call", inj primCall)

          , ("-", inj primSub)

          , ("nROT", inj primNRot)
          , ("nDUP", inj primNDup)
          , ("nDROP", inj primNDrop)
          , ("nROT.c", inj primNRotC)
          , ("nDUP.c", inj primNDupC)
          , ("nDROP.c", inj primNDropC)
          , ("popC", inj primPopCtrl)
          , ("pushC", inj primPushCtrl)

          , ("DEBUG", inj primDebugData)
          ]
        }
  let progText = unlines
        [ ":fun _start testloop funkexit ;"
        , ":fun say 65 put.b 10 put.b ;"

        , ":fun funkexit"
        ,   "' exit DUP"
        ,   "0 42 1 ifnz"
        ,   "ROT call"
        , ";"

        -- (i) -> (i-1)
        , ":fun testloop.body say 1 - ;"
        -- () -> ()
        , ":fun testloop"
        ,   "3 ' testloop.body ' DUP"
            -- (ctr, body, test)
        ,   "while"
            -- (0)
        ,   "1 nDROP"
            -- ()
        , ";"

        -- here's the hypothesis: that I can use the control stack manipulations with ifnz to get loops
        -- NOTE and it freaking works!!!!!! yooooooo! let's gooooooo!!!!!!
        -- (body, test) -> ()
        , ":fun while"
        ,   "SWAP pushC DUP pushC"
            -- (test){body, test}
        ,   "call"
            -- (cond){body, test}
        ,   "' while.onFalse ' while.onTrue ROT"
            -- (onFalse, onTrue, cond){body, test}
        ,   "ifnz"
            -- (handler){body, test}
        ,   "jump" -- tail-call into either the body wrapper or the while cleanup
        , ";"
        -- (){body, test} -> ()
        , ":fun while.onTrue"
        ,   "2 nDUP.c popC"
            -- (body){body, test}
        ,   "call"
            -- (){body, test}
        ,   "popC popC SWAP"
            -- (body, test)
        ,   "' while jump" -- tail-call while to continue the test-body loop
        , ";"
        -- (){body, test} -> ()
        , ":fun while.onFalse"
        ,   "1 nDROP.c"
        ,   "1 nDROP.c"
        , ";"

        , ":fun DUP 1 nDUP ;"
        , ":fun ROT 3 nROT ;"
        , ":fun SWAP 2 nROT ;"
        , ":fun NOP ;"
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
  , cStack :: ![Value] -- NOTE this will normally be ProgramCounter, but it could hold function pointers, and there's no reason not to hold ordinary values as long as we don't try to jump to them
  , code :: ![(String, Thread)]
  , mem :: ![(String, Variable)]
  }

data Value
  = Data Integer
  | Ptr String
  | IPtr ProgramCounter
  | VPtr (String, Int) -- varname + offset
  deriving (Show)

type Variable = [Value]

data Thread
  = PT PrimThread
  | UT UserThread
data PrimThread = PrimThread { prim :: !(Forth ()) }
data UserThread = UserThread { threadName :: !String, instrs :: ![Instr] }
data Instr
  = Subr String
  | Imm Value
  | Return -- FIXME I dunno why I couldn't GADT this one to make it not constructable in actual lists of Instrs
  -- NOTE I thought about including "skip over a function that is defined here",
  -- but I decided againt is because that's harder to inspect the assembly/machine code for.
  deriving(Show)

------ execution ------

newtype Forth a = F { unF :: StateT Machine IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

data ProgramCounter
  = UPC (UserThread, Int)
  | PPC PrimThread
instance Show ProgramCounter where
  show (UPC (thd, off)) = "UPC " ++ show thd.threadName <> " + " <> show off
  show  (PPC _) = "PPC <unknown>"

step :: Forth ()
step = do
  getPc >>= \case
    UPC (thd, off) -> do
      op <- case thd.instrs !? off of
        Just op -> do
          setPc $ UPC (thd, off+1)
          pure op
        Nothing -> pure Return
      case op of
        Subr name -> findThread name >>= callSubr
        Imm v -> pushData v
        Return -> retSubr
    PPC op -> op.prim >> retSubr

callCont :: ProgramCounter -> Forth ()
callCont k = do
  pushCtrl =<< IPtr <$> getPc
  setPc k
callSubr :: Thread -> Forth ()
callSubr (UT thd) = callCont $ UPC (thd, 0)
callSubr (PT subr) = subr.prim

retSubr :: Forth ()
retSubr = do
  k <- popCtrl >>= \case
    -- FIXME if popCtrl fails, I want to just exit the program with either TOS or else success
    IPtr it -> pure it
    Ptr name -> findThread name >>= \case
      UT it -> pure $ UPC (it, 0)
      PT op -> pure $ PPC op
    other -> error $ "type error: expecting continuation, got: " <> show other
  setPc k

------ support ------

findThread :: String -> Forth Thread
findThread name = F $ do
  tbl <- gets (.code)
  case lookup name tbl of
    Just thd -> pure thd
    Nothing -> error $ "no such subroutine: " <> show name

withData :: ([Value] -> (a, [Value])) -> Forth a
withData f = F $ do
  (a, xs) <- gets $ f . (.dStack)
  modify $ \m -> m{dStack = xs}
  pure a
-- common withData operations
pushData :: Value -> Forth ()
pushData v = withData $ \stack -> ((), v:stack)
popData :: Forth Value
popData = withData $ fromMaybe (error "stack underflow") . uncons

withCtrl :: ([Value] -> (a, [Value])) -> Forth a
withCtrl f = F $ do
  (a, xs) <- gets $ f . (.cStack)
  modify $ \m -> m{cStack = xs}
  pure a
-- common withCtrl operations
pushCtrl :: Value -> Forth ()
pushCtrl v = withCtrl $ \stack -> ((), v:stack)
popCtrl :: Forth Value
popCtrl = withCtrl $ fromMaybe (error "control stack underflow") . uncons

getPc :: Forth ProgramCounter
getPc = F $ gets (.pc)
setPc :: ProgramCounter -> Forth ()
setPc target = F $ modify $ \m -> m{pc = target}

expectInt :: String -> Value -> Forth Integer
expectInt _ (Data i) = pure i
expectInt msg other = error $ msg <> ":\n\ttype error: expecing int, got " <> show other

expectIndex :: String -> Value -> Forth Int
expectIndex _ (Data i) | fromInteger @Int i > 0 = pure $ fromInteger i - 1
expectIndex msg other = error $ msg <> "\n\ttype error: expecing positive int, got " <> show other

expectCont :: Value -> Forth ProgramCounter
expectCont (Ptr name) = findThread name >>= \case
  UT thd -> pure $ UPC (thd, 0)
  PT op -> pure $ PPC op
expectCont (IPtr it) = pure it
expectCont other = error $ "type error: expected function pointer, got " <> show other

------------------------
------ primitives ------
------------------------

primDebugData :: Forth ()
primDebugData = do
  stack <- F $ gets (.dStack)
  liftIO $ print stack

-- NOTE if the machine were implemented in assembly, these would also be,
-- though perhaps as a library that can be linked with the compiler output

------ stack manipulation ------

-- TODO: nmove -- pop tos and replace n entries down (from now) with the result
-- TODO: npush -- pop tos and insert it so that it is n entries down, moving everything else up

-- TODO: n{peek,drop,&c}.c -- same stuff but act on the control stack
-- TODO: n{peek,pop}.cd -- same stuff but move values from control to data
-- TODO: n{peek,pop}.dc -- same stuff but move values from data to control
-- TODO: or the motion ones could be save, restore, look, idk

--- the data stack ---

primNDup :: Forth ()
primNDup = do
  i <- popData >>= expectIndex "ndup"
  v <- withData $ \stack -> (fromMaybe (error "stack underflow") (stack !? i), stack)
  pushData v

primNRot :: Forth ()
primNRot = do
  i <- popData >>= expectIndex "nrot"
  v <- withData $ \stack -> fromMaybe (error "stack underflow") (stack !< i)
  pushData v

primNDrop :: Forth ()
primNDrop = do
  i <- popData >>= expectIndex "ndrop"
  _ <- withData $ \stack -> fromMaybe (error "stack underflow") (stack !< i)
  pure ()

--- the control stack ---

primNDupC :: Forth ()
primNDupC = do
  i <- popData >>= expectIndex "ndup.c"
  v <- withCtrl $ \stack -> (fromMaybe (error "control stack underflow") (stack !? i), stack)
  pushCtrl v

primNRotC :: Forth ()
primNRotC = do
  i <- popData >>= expectIndex "nrot.c"
  v <- withCtrl $ \stack -> fromMaybe (error "control stack underflow") (stack !< i)
  pushCtrl v

primNDropC :: Forth ()
primNDropC = do
  i <- popData >>= expectIndex "ndrop.c"
  _ <- withCtrl $ \stack -> fromMaybe (error "control stack underflow") (stack !< i)
  pure ()

--- between stacks ---

primPopCtrl :: Forth ()
primPopCtrl = popCtrl >>= pushData

primPushCtrl :: Forth ()
primPushCtrl = popData >>= pushCtrl

------ control flow ------

-- TODO: if, probably while and maybe some other basic control flow
-- (when/unless, fold, do-while, idk, perhaps somehow switch or cond)

primJump :: Forth ()
primJump = popData >>= expectCont >>= setPc

primCall :: Forth ()
primCall = popData >>= expectCont >>= callCont

primIfnz :: Forth ()
primIfnz = do
  cond <- popData >>= expectInt "ifnz condition"
  onNz <- popData
  onZ <- popData
  pushData $ if cond == 0 then onZ else onNz

------ arithmetic and logic ------

-- TODO: add, sub, {i,u}mul, {i,u}{div,mod,divmod}, neg
-- TODO: shl, {i,u}shr
-- TODO: and, or, xor, not
-- TODO: cmp, {n,}eq, {i,u}{l,g}{t,e}
-- TODO: something about wide/carry/borrow arithmetic

primSub :: Forth ()
primSub = do
  minuend <- popData >>= expectInt "-"
  subtrahend <- popData >>= expectInt "-"
  pushData $ Data (subtrahend - minuend)

------ environment ------

primExit :: Forth ()
primExit = popData >>= expectInt "exit" >>= \case
  0 -> liftIO exitSuccess
  err -> liftIO $ exitWith (ExitFailure $ fromInteger err)

primPutStdout :: Forth ()
primPutStdout = do
  tos <- popData >>= expectInt "put"
  liftIO $ BS.putStr . BS.singleton $ fromInteger tos


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
    { pc = UPC (thd0, 0)
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
        { funs = (name, inj $ UserThread name body) : acc.funs
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
  fromString str = UT . UserThread "<unknown>" $ fromString <$> words str

class Inj to from where inj :: from -> to

instance Inj PrimThread (Forth ()) where inj = PrimThread
instance Inj Thread PrimThread where inj = PT
instance Inj Thread (Forth ()) where inj = inj . inj @PrimThread
instance Inj UserThread [Instr] where inj = UserThread "<unknown>"
instance Inj Thread UserThread where inj = UT
instance Inj Thread [Instr] where inj = inj . inj @UserThread

-- indexes into the list and also removes the found element
infixl 9 !<
(!<) :: [a] -> Int -> Maybe (a, [a])
xs0 !< i0
  | i0 >= 0 = loop [] xs0 i0
  | otherwise = Nothing
  where
  loop pre (x:post) 0 = Just (x, reverse pre <> post)
  loop pre (x:post) i = loop (x:pre) post (i-1)
  loop _ [] _ = Nothing

