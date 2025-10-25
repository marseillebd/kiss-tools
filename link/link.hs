-- Binary Assembler In Haskell
-- ===========================
--
-- Haskell is such a high-level language that it might not seem reasonable to write a "stupid simple" tool in it.
-- How is anyone supposed to translate garbage-collected, pure functional code into low-level machine code?
-- The fact is, I'm writing this Haskell in a very straightforward way so that it the translation should be relatively easy.
-- Tail recursion becomes loops, inner functions inline to blocks, and concats of endomorphisms become iterative mutation.
--
-- The main advantage Haskell has is that it is already close in form to the specification.
-- Thus, it should be easy to read the Haskell next to the standard and observe that the code is correct directly.
-- Additionally, Haskell has _iffy_ support for regular expressions, so it's easy to avoid dependency on a regex engine.
-- I don't know about you, but I'd rather not build a regex engine before I have an assembler.
--
-- For those of you unfamiliar with Haskell, I recommend the Haskell Wiki's article
-- [How to Read Haskell](https://wiki.haskell.org/How_to_read_Haskell).
-- It gives a good overview for how to understand what's going on here, without having you also learn how to write Haskell.
-- Beyond that, use the standard as your guide to my intentions through the code.
--
-- For those of you who are familair with Haskell, yes, I know I'm being unidiomatic.
-- I primarily want to avoid dependencies beyond base (including anything accidentally in base which should actually be in a dedicated ghc package).
-- A Python program, as slow as that language is, would be sufficient for performance.
-- Haskell's built-in strings and lists would also be sufficient, if you were to compile this code with ghc.
-- If someone hand-compiles it, as intended, the Prelude's performace problems go away,
-- and they'd appreciate not having to hand-compile any other packages.

-- The Boring Stuff
-- ================
--
-- Skip this on first read.
-- It's just imports and helpers that improve on some poor design decisions.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Bits (Bits((.&.), (.|.), shiftL, shiftR, testBit))
import Data.Char (chr, ord, toLower)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (isNothing)
import Data.Word (Word8)
import System.Exit (exitFailure)

intCast :: (Integral a, Integral b) => a -> b
intCast = fromIntegral

strtok :: Char -> String -> [String]
strtok c = go
  where
  go "" = []
  go str = case span (/= c) str of
    ("", rest) -> [rest]
    (x, rest) -> x : go rest

die :: (Show msg) => msg -> IO a
die msg = print msg >> exitFailure -- TODO should print to stderr

-- Hardcoded Configuration
-- =======================

-- Keep it simple!
-- Why write and compile a bunch of code to manage option parsing and validation when the user is expected to be able to hand-compile from source?
-- I've written this for 8-bit addressible data encoded in hexadecimal.
-- If you need a different format, these are easy function/values to recode.

-- Should specify number of bits per byte.
-- A byte defined as filling the bitspace between two consecutive addresses.
-- On most modern machines, this is eight bits, but you might need to target oddball machines.
-- I'm personally partial to the "9-bit byte", so if I get a simulator for such a machine, this is the number I'd change to compensate.
bitsPerByte :: Int
bitsPerByte = 8

type Byte = Word8 -- TODO make a newtype

-- Should (effectively) specify the base used to encode bytes in text (in the source).
-- For exampe, a hexidecimal digit can hold four bits, whereas an octal digit holds three.
-- If you need a non-integral number of bits per digit (e.g. decimal), delete this value and modify `digitsPerByte` and `spanByte` specially.
bitsPerDigit :: Int
bitsPerDigit = 4

splitDigit :: String -> Maybe (Int, String)
splitDigit (c:rest)
  | '0' <= c && c <= '9' = pure (ord c - ord '0', rest)
  | 'A' <= c && c <= 'F' = pure (ord c - ord 'A' + 10, rest)
  | 'a' <= c && c <= 'f' = pure (ord c - ord 'a' + 10, rest)
  | otherwise = Nothing
splitDigit "" = Nothing

renderDigit :: Byte -> Char
renderDigit b
  | b <= 9 = chr (ord '0' + intCast b)
  | 10 <= b && b <= 15 = chr (ord 'A' -10 + intCast b)
  | otherwise = undefined

-- TODO render byte as binary
-- if cross-linking, I'd suggest a bitstream, which would change some architecture


-- This is the storage type for the results of expression computation, and so determined the maximum patch size
-- It needs to be signed, because bit testing might not do 2s-complement
type Value = Int -- TODO make it a newtype

-- TODO a bit test function for Value

-- Adapt Configuration to Functions
-- --------------------------------
--
-- Instead of using the "config" directly, we wrap them into slightly-higher-level operations for use in the rest of the program.
-- Effectively, these make it easier to swap out some of the implementation.

digitsPerByte :: Int
digitsPerByte = case bitsPerByte `divMod` bitsPerDigit of
  (n, 0) -> n
  (n, _) -> n + 1

splitByte :: String -> Maybe (Byte, String)
splitByte = go digitsPerByte 0
  where
  go 0 acc rest = pure (intCast acc, rest)
  go i acc str = do
    (digitVal, rest) <- splitDigit str
    go (i - 1) (shiftL acc bitsPerDigit + digitVal) rest

renderByte :: Byte -> String
renderByte b = reverse [renderDigit $ (b `shiftR` (i * intCast bitsPerDigit)) .&. intCast lowDigitMask | i <- [0 .. digitsPerByte-1]]

lowDigitMask :: Value
lowDigitMask = (1 `shiftL` bitsPerDigit) - 1

lowByteMask :: Value
lowByteMask = (1 `shiftL` bitsPerByte) - 1

-- Entry Point
-- ===========

main :: IO ()
main = do
  -- TODO parse cmdline args
  input <- getContents
  -- parse the whole file
  unevald <- case parseTokens input of
    Right it -> pure it
    Left err -> die err
  -- TODO evaluate expressions
  let unpatched = unevald
  -- patch the payload
  patchedPayload <- case runPatch unpatched of
    Right ok -> pure ok
    Left err -> die (show err)
  -- TODO output the result
  -- TODO everything after this is hacky test stuff
  let displayText :: Payload -> String
      displayText (Byte n) = renderByte n
      displayText (Ws ws) = ws
  let debugPattern = intercalate "," . fmap (maybe "x" show)
  putStrLn "LAYOUTS:"
  forM_ unevald.layouts $ \(name, layout) -> putStrLn $ name ++ ": " ++ debugPattern layout.pattern
  putStrLn "VALUES:"
  forM_ unpatched.values $ \(name, v) -> putStrLn $ name ++ ": " ++ show v
  putStrLn "PATCHES:"
  forM_ unevald.patches $ \patch -> putStrLn $ show patch.offset ++ " <-(" ++ debugPattern patch.layout.pattern ++ ") " ++ show patch.expr
  putStrLn "BODY:"
  -- putStr $ concatMap displayText unevald.payload
  -- putStrLn ">>>>>>>>>>>>"
  putStr $ concatMap displayText patchedPayload
  -- putStrLn "============"
  -- print unevald

-- TODO do I read the payload into memory or write it to a file
-- (and then patch before writing anything to disk, or write as I go and use fseek to patch?)
-- for now, I'll write to memory, because that's easier

data Accum = Accum
  { offset :: !Offset
  , namespace :: String
  , defaultLayout :: Maybe Layout
  , layouts :: [(Name, Layout)]
  , payload :: [Payload]
  , exprs :: [(Name, Expr)]
  , values :: [(Name, Value)]
  , patches :: [Patch]
  }
  deriving (Show)

emptyAccum :: Accum
emptyAccum = Accum
  { offset = 0
  , namespace = ""
  , defaultLayout = Nothing
  , layouts = []
  , payload = []
  , exprs = []
  , values = []
  , patches = []
  }

-- Directives
-- ==========

parseDirective :: Name -> String -> Accum -> Either LinkError Accum
  -- TODO more directives (pad, align, let & perhaps also var)
parseDirective "namespace" str0 acc = do
  ns <- case splitId str0 of
    Just (name, "") -> pure name
    Nothing -> pure ""
    Just (_, rest) -> Left $ SyntaxError "unexpected tokens after namespace directive:" rest
  pure acc{namespace = ns}
parseDirective "defaultlayout" str0 acc = do
  name <- case splitId str0 of
    Just (name, "") -> pure name
    Just (_, rest) -> Left $ SyntaxError "unexpected tokens after defaultLayout name" rest
    Nothing -> Left $ SyntaxError "execting name for defaultLayout" str0
  layout <- case lookup name acc.layouts of
    Just it -> pure it
    Nothing -> Left $ UnknownLayout name
  case acc.defaultLayout of
    Nothing -> pure ()
    Just _ -> Left DoubleDefaultDefinition
  pure acc{defaultLayout = Just layout}
parseDirective "layout" str0 acc = do
  (name, str1) <- case splitId str0 of
    Just it -> pure it
    _ -> Left $ SyntaxError "expecting layout name" str0
  str2 <- case splitWs str1 of
    Just (_, it) -> pure it
    Nothing -> Left $ SyntaxError "expecting space after layout name" str1
  pattern <- parseLayout str2
  let lenBad =
           length pattern `mod` bitsPerByte /= 0
        || null pattern -- i.e. length is zero
        -- TODO pattern too long to store in `Value`
  when lenBad $ Left BadLayoutSize
  let layout = Layout
        { pattern = pattern
        , nBytes = length pattern `div` bitsPerByte
        }
  pure acc
    { layouts = (name, layout) : acc.layouts
    }
parseDirective name _ _ = Left $ UnknownDirective name

-- Layouts
-- =======

data Layout = Layout
  { pattern :: [Maybe Int]
  , nBytes :: !Int
  }
  deriving (Show)

parseLayout :: String -> Either LinkError [Maybe Int]
parseLayout str0 = if
  -- contiguous bit range
  | Just (a, '-':str1) <- splitInt str0
  , Just (b, rest) <- splitInt str1
  -> do
    let bits = Just <$> if a <= b then [a .. b] else [a, a - 1 .. b]
    (bits ++) <$> parseLayout rest
  -- single bit index
  | Just (a, rest) <- splitInt str0
  -> (Just a :) <$> parseLayout rest
  -- multiple padding bits
  | (c:'*':str1) <- str0
  , c `elem` "xX"
  , Just (n, rest) <- splitInt str1
  -> (replicate n Nothing ++) <$> parseLayout rest
  -- single padding bit
  | (c:rest) <- str0
  , c `elem` "xX"
  -> (Nothing :) <$> parseLayout rest
  -- whitespace
  | Just (_, rest) <- splitWs str0
  -> parseLayout rest
  -- end of layout
  | null str0 -> pure []
  -- anything else
  | otherwise -> Left $ SyntaxError "unexpected tokens in layout definition" str0

-- Patches
-- =======

data Patch = Patch
  { offset :: Offset
  , layout :: Layout
  , expr :: Expr
  }
  deriving (Show)

splitPatch :: String -> Maybe (String, String)
splitPatch ('{':input) = do
  let (inner, input') = span (/= '}') input
  case input' of
    ('}':rest) -> pure (inner, rest)
    _ -> Nothing
splitPatch _ = Nothing

parsePatch :: Name -> String -> Either LinkError (Maybe Name, Maybe [Payload], Expr)
parsePatch ns inner = case strtok ';' inner of
  [exprStr] -> do
    expr <- parseExpr ns exprStr
    pure (Nothing, Nothing, expr)
  [layoutStr, exprStr] -> do
    layoutName <- parseLayoutName layoutStr
    expr <- parseExpr ns exprStr
    pure (Just layoutName, Nothing, expr)
  [layoutStr, bytesStr, exprStr] -> do
    layoutName <- parseLayoutName layoutStr
    base <- parseBytes bytesStr
    expr <- parseExpr ns exprStr
    pure (Just layoutName, Just base, expr)
  _ -> Left $ SyntaxError "expecting 1-3 semicolon-delimited sections in patch" inner
  where
  parseLayoutName str = case splitId (trim str) of
    Just (name, "") -> pure name
    _ -> Left $ SyntaxError "expected layout name" str
  parseBytes str = fmap reverse $ parseMany [] str $ \bytes input -> if
    -- plain byte
    | Just (byte, rest) <- splitByte input
    -> pure (Byte byte : bytes, rest)
    -- whitespace
    | Just (ws, rest) <- splitWs input
    -> pure (Ws ws : bytes, rest)
    | otherwise -> Left $ SyntaxError "unexpected tokens in patch base" input

runPatch :: Accum -> Either LinkError [Payload]
runPatch accum = go 0 accum.patches accum.payload
  where
  go :: Int -> [Patch] -> [Payload] -> Either LinkError [Payload]
  go !_ [] payload = pure payload
  go !i (p:ps) payload | p.offset == i = do
    -- The names `@` and `@@` are reserved for the start/end of the patch.
    -- The memnonic is that "at" means "here" and the longer one is for the larger (later) side of here.
    let env = [("@", p.offset), ("@@", p.offset + p.layout.nBytes)] ++ accum.values
    value <- eval env p.expr >>= \case
      Val v -> pure v
      other -> Left $ IncompleteEvaluation other
    let payload' = patch1 p.layout value payload
    go i ps payload'
  go !i ps (Byte b : rest) = (Byte b :) <$> go (i + 1) ps rest
  go !i ps (Ws ws : rest) = (Ws ws :) <$> go i ps rest
  go !_ _ [] = error "internal error"

patch1 :: Layout -> Value -> [Payload] -> [Payload]
patch1 layout value base =
  let baseValue = loadBase layout.nBytes base
      patchedValue = patchValue baseValue layout value
      patchedBytes = valueToBytes layout patchedValue
   in writePatch patchedBytes base

loadBase :: Int -> [Payload] -> Value
loadBase = go 0
  where
  go !acc 0 _ = acc
  go !acc !i (Byte x : rest) = go ((acc `shiftL` bitsPerByte) + intCast x) (i - 1) rest
  go !acc !i (_ : rest) = go acc i rest
  go !acc !i [] = go acc i (repeat $ Byte 0)

writePatch :: [Byte] -> [Payload] -> [Payload]
writePatch [] rest = rest
writePatch bs [] = Byte <$> bs
writePatch (b:bs) (Byte _ : rest) = Byte b : writePatch bs rest
writePatch bs (other : rest) = other : writePatch bs rest

patchValue :: Value -> Layout -> Value -> Value
patchValue base layout value =
  let spreadValue = fromBools 0 [maybe False (value `testBit`) p | p <- layout.pattern]
      mask = fromBools 0 [isNothing p | p <- layout.pattern]
  in (mask .&. base) .|. spreadValue
  where
  fromBools :: Value -> [Bool] -> Value
  fromBools acc [] = acc
  fromBools acc (b:rest) = fromBools ((acc `shiftL` 1) + (if b then 1 else 0)) rest

valueToBytes :: Layout -> Value -> [Byte]
valueToBytes layout value = [intCast $ getByte i | i <- byteIxs]
  where
  getByte i = (value `shiftR` (i*bitsPerByte)) .&. lowByteMask
  byteIxs = reverse [0..layout.nBytes - 1]

-- Expressions
-- ===========

data Expr
  = Val Value
  | Var Name
  | Func Name [Expr]
  | Arith Expr [(BinOp,Expr)]
  deriving (Show)

data BinOp
  = Add
  | Sub
  | Mul
  deriving (Show)

-- ```ebnf
-- expr = arithExpr ;
-- arithExpr = [ws] simpleExpr [ [ws] binOp arithExpr] ;
-- binOp = '+' | '-' | '*' ;
-- simpleExpr
--   = number
--   | identifier '(' [ws] [argsList [ws]] ')'
--   | identifier
--   | '(' arithExpr [ws] ')' ;
-- argsList = arithExpr [ [ws] ',' [ws] argsList ]
-- ```

parseExpr :: Name -> String -> Either LinkError Expr
parseExpr ns input0 = do
  (e, rest) <- expr input0
  unless (null $ dropWs rest) $
    Left $ SyntaxError "unexpected tokens after expression" rest
  pure e
  where

  expr :: String -> Either LinkError (Expr, String)
  expr input1 = do
    (e, tl, rest) <- arithExpr input1
    case tl of
      [] -> pure (e, rest)
      _ -> pure (Arith e tl, rest)

  arithExpr :: String -> Either LinkError (Expr, [(BinOp, Expr)], String)
  arithExpr input1 = do
    let input2 = dropWs input1
    (e, rest) <- simpleExpr input2
    if
      | input3 <- dropWs rest
      , Just (op, input4) <- binOp input3
      -> do
        (e', tl, rest') <- arithExpr input4
        pure (e, (op,e'):tl, rest')
      | otherwise
      -> pure (e, [], rest)

  binOp :: String -> Maybe (BinOp, String)
  binOp (c:rest)
    | Just op <- lookup c [('+', Add), ('-', Sub), ('*', Mul)]
    = pure (op, rest)
  binOp _ = Nothing

  simpleExpr :: String -> Either LinkError (Expr, String)
  simpleExpr input1
    -- literals
    | (n@(_:_), rest) <- span (`inClass` "0-9") input1
    = pure (Val $ read n, rest)
    -- functions
    | Just (name, input2) <- splitId input1
    , ('(':input3) <- input2
    , input4 <- dropWs input3
    = case input4 of
        (')': rest) -> pure (Func name [], rest)
        _ -> do
          (args, rest) <- argsList input4
          case dropWs rest of
            (')':rest') -> pure (Func name args, rest')
            other -> Left $ SyntaxError "expected close paren" other
    -- variables
    | Just (x, rest) <- splitId input1
    , qualX <- case x of { ('.':_) -> ns ++ x; _ -> x } -- add namespace when applicable
    = pure (Var qualX, rest)
    -- parens
    | ('(':input2) <- input1
    = do
      (e, rest) <- expr input2
      case rest of
        (')':rest') -> pure (e, rest')
        other -> Left $ SyntaxError "expecting close parenthesis" other
    | "" <- input1
    = Left $ SyntaxError "unexpeted end of input" input1
    | otherwise
    = Left $ SyntaxError "unexpected tokens in expression" input1

  argsList :: String -> Either LinkError ([Expr], String)
  argsList input1 = do
    (e, rest) <- expr input1
    case dropWs rest of
      (',':input2) -> do
        let input3 = dropWs input2
        (es, rest2) <- argsList input3
        pure (e:es, rest2)
      _ -> pure ([e], rest)

eval :: [(Name, Value)] -> Expr -> Either LinkError Expr
eval _ (Val v) = pure $ Val v
eval env (Var x) = case lookup x env of
  Just v -> pure $ Val v
  Nothing -> pure $ Var x
eval env (Func name args) = do
  f <- case lookup name builtins of
    Just f -> pure f
    Nothing -> Left $ UnknownFunction name
  args' <- mapM (eval env) args
  case fromValues args' of
    Nothing -> pure $ Func name args'
    Just vals -> Val <$> f vals
eval env (Arith e0 eOps) = do
  let (ops0, es0) = unzip eOps
  e0' <- eval env e0
  es0' <- mapM (eval env) es0
  case fromValues (e0':es0') of
    Nothing -> pure $ Arith e0' (zip ops0 es0')
    Just [] -> error "internal error"
    Just (v0:vs0) -> do
      let (v', vs', ops') = case doMuls ([], []) (v0:vs0) ops0 of
            (a:b, c) -> (a, b, c)
            _ -> error "internal error"
          !v'' = v' + doSum vs' ops'
      pure $ Val v''
  where
  doMuls (revEs, revOps) [e] [] = (reverse (e:revEs), reverse revOps)
  doMuls !revacc (a:b:es) (Mul:ops) = doMuls revacc (a*b : es) ops
  doMuls (revEs, revOps) (a:es) (op:ops) = doMuls (a:revEs, op:revOps) es ops
  doMuls _ [] _ = error "internal error"
  doMuls _ (_:_:_) [] = error "internal error"
  doSum (v:vs) (Add:ops) = v + doSum vs ops
  doSum (v:vs) (Sub:ops) = doSum vs ops - v
  doSum (_:_) (Mul:_) = error "internal error"
  doSum [] [] = 0
  doSum _ _ = error "internal error"

fromValues :: [Expr] -> Maybe [Value]
fromValues = mapM fromValue

fromValue :: Expr -> Maybe Value
fromValue (Val v) = Just v
fromValue _ = Nothing

builtins :: [(Name, [Value] -> Either LinkError Value)]
builtins = [] -- TODO

-- Payload
-- =======

data Payload
  = Byte Byte
  | Ws String
  deriving(Show)

parseTokens :: String -> Either LinkError Accum
parseTokens input = do
  revacc <- parseMany emptyAccum input parseToken
  pure revacc
    { payload = reverse revacc.payload
    , patches = reverse revacc.patches -- so that patch points are in acending order by offset
    }

parseToken :: Accum -> String -> Either LinkError (Accum, String)
parseToken acc input = if
  -- plain byte
  | Just (byte, rest) <- splitByte input
  -> let acc' = (acc :: Accum)
          { offset = acc.offset + 1
          , payload = Byte byte:acc.payload
          }
     in  pure (acc', rest)
  -- labels
  | Just (name, input') <- splitId input
  , (':':rest0) <- input'
  , rest <- dropWhile (`elem` " \t") rest0
  , fullname <- case name of
      ('.':_) -> acc.namespace ++ name
      _ -> name
  -> pure (acc{values = (fullname, acc.offset):acc.values}, rest)
  -- patch point
  | Just (inner, rest) <- splitPatch input
  -> do
    (layout_m, base_m, expr) <- parsePatch acc.namespace inner
    layout <- case layout_m of
      Just layoutName -> case lookup layoutName acc.layouts of
        Just layout -> pure layout
        Nothing -> Left $ UnknownLayout layoutName
      Nothing -> case acc.defaultLayout of
        Just layout -> pure layout
        Nothing -> Left MissingDefaultLayout
    base <- case base_m of
      Just base -> pure base
      Nothing -> pure $ replicate layout.nBytes (Byte 0)
    let patch = Patch
          { offset = acc.offset
          , layout = layout
          , expr = expr
          }
    let acc' = acc
          { offset = acc.offset + length base
          , payload = reverse base ++ acc.payload
          , patches = patch : acc.patches
          }
    pure (acc', rest)
  -- directives
  | (':':input') <- input
  , Just (name, input'') <- splitSimpleId input'
  , (line, rest) <- span (`notElem` "\n\r") input''
  -> do
    acc' <- parseDirective (toLower <$> name) (trim line) acc
    pure (acc', rest)
  -- comments
  | ('#':_) <- input
  , (comment, rest) <- span (`notElem` "\n\r") input
  -> pure (acc{payload = Ws comment:acc.payload}, rest)
  -- whitespace and end of line
  | "" <- input
  -> pure (acc{payload = Ws "\n":acc.payload}, "")
  | Just (ws, rest) <- splitWs input
  -> pure (acc{payload = Ws ws:acc.payload}, rest)
  -- syntax error
  | otherwise
  -> Left $ SyntaxError "bad token" input

-- Support
-- =======

type Name = String -- TODO should be a newtype

type Offset = Int -- TODO should be unsigned, and perhaps a newtype

-- Parsing
-- -------

parseMany :: s -> String -> (s -> String -> Either LinkError (s, String)) -> Either LinkError s
parseMany st0 str0 f = go st0 str0
  where
  go st "" = pure st
  go st str = do
    (st', rest) <- f st str
    go st' rest

trim :: String -> String
trim = dropWhileEnd (`elem` " \t") . dropWhile (`elem` " \t")

dropWs :: String -> String
dropWs = dropWhile (`elem` " \t\n\r")

splitWs :: String -> Maybe (String, String)
splitWs str = case span (`elem` " \t\n\r") str of
  ("", _) -> Nothing
  it -> Just it

splitInt :: String -> Maybe (Int, String)
splitInt str = case span (`inClass` "0-9") str of
  ("", _) -> Nothing
  (n, rest) -> Just (read n, rest)

splitSimpleId :: String -> Maybe (String, String)
splitSimpleId [] = Nothing
splitSimpleId (c:tl)
  | c `inClass` simpleIdStart
  , (cs, rest) <- span (`inClass` idBody simpleIdStart) tl
  = Just (c:cs, rest)
  | otherwise = Nothing

splitId :: String -> Maybe (String, String)
splitId [] = Nothing
splitId (c:tl)
  | c `inClass` idStart
  , (cs, rest) <- span (`inClass` idBody idStart) tl
  = Just (c:cs, rest)
  | otherwise = Nothing

simpleIdStart :: String
simpleIdStart = "a-zA-Z"

idStart :: String
idStart = simpleIdStart ++ "@$^_.<>"

idBody :: String -> String
idBody = ("0-9" ++)

inClass :: Char -> String -> Bool
inClass c = go
  where
  go (a:'-':b:rest) = (a <= c && c <= b) || go rest
  go (a:rest) = c == a || go rest
  go [] = False

-- Errors
-- ------

data LinkError
  = SyntaxError String String -- message, then remaining string
  | MissingDefaultLayout
  | UnknownLayout Name
  | UnknownDirective Name
  | DoubleDefaultDefinition
  | BadLayoutSize
  | IncompleteEvaluation Expr
  | UnknownFunction Name
  | BadArguments [Expr]
  | StackedError LinkError LinkError
  deriving(Show)
