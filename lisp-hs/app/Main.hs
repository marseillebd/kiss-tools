{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Streaming (MonadIO(..))
import Streaming.Prelude (Stream, Of(..))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  let src = "(call printLnInt (call add 2 5))\
            \(call (call (fn (x) (fn (x) x)) 1) 2)" :: String
  S.print $ tokenize $ charSpans Nothing (Off 1 1) $ S.each src
  putStrLn ""
  printNodes $ reader $ tokenize $ charSpans Nothing (Off 1 1) $ S.each src
  putStrLn ""
  (ast :> ()) <- toConcreteSyntax $ reader $ tokenize $ charSpans Nothing (Off 1 1) $ S.each src
  mapM_ print $ toUexpr <$> ast
  putStrLn ""
  forM_ ast $ \expr -> do
    ueval u_startEnv (toUexpr expr) >>= print

-----------------------
------ Locations ------
-----------------------

data Off = Off
  -- , byte :: !Int -- 0-indexed -- TODO after I implement my own decode stream
  { line :: !Int -- 1-indexed
  , col :: !Int -- 1-indexed
  }
  deriving (Show)

data Span = Span
  { file :: Maybe FilePath -- TODO probably use a type that can distinguish, say stdin from repl lines?
  , start :: Off
  , end :: Off
  }
  deriving (Show)

data Loc = Loc (NonEmpty Span)
  deriving (Show)

charSpans :: Monad m => Maybe FilePath -> Off -> Stream (Of Char) m r -> CharStream m r
charSpans f0 off0 = S.scanned update (Span f0 off0 off0) id
  where
  update (Span f _ off) c = Span f off (off `advChar` c)
  advChar :: Off -> Char -> Off
  advChar off '\n' = off{line = off.line+1, col = 1}
  advChar off _ = off{col = off.col+1}

--------------------
------ Tokens ------
--------------------

------ Language ------

-- TODO
-- What I'm noticing is that I'm likely just going to panic as soon as there's a parse error, at least for this first draft.
-- I think a polished form would just use nanopass to create raw tokens, progressively remove the raws I don't need anymore,
-- and only panic when there are inserted error correction tokens left on translation to the processed tokens I've defined here.

data Token
  = Punctuation Span PunctToken
  | Symbol Span Text -- TODO use a Symbol newtype
  | IntLit Span Integer
  -- | SciLit -- TODO
  | StrLit StringToken
  deriving (Show)

data PunctToken
  = OpenParen | CloseParen
  deriving (Eq, Show)

data StringToken = StrToken
  { openStr :: (Span, StrQuote)
  , insideStr :: [(Span, StringPart)] -- TODO use an array type
  , closeStr :: (Span, StrQuote)
  }
  deriving (Show)

data StringPart
  = PlainStr Text
  | Escape Text
  deriving (Show)

data StrQuote = DblQuote | Backtick
  deriving (Eq, Show)

------ Errors ------

class (Monad m) => TokenizeError m where

instance TokenizeError IO where -- DEBUG

------ Algorithm ------

type CharStream m r = Stream (Of (Char, Span)) m r
type TokStream m r = Stream (Of Token) m r

tokenize :: forall m. (TokenizeError m) => CharStream m () -> TokStream m ()
tokenize s = S.effect $ S.next s <&> \case
  Left done -> pure done
  Right more -> S.effect $ uncurry loop <$> dispatch more
  where
  dispatch :: ((Char, Span), CharStream m ()) -> m (Maybe Token, CharStream m ())
  dispatch ((x, lx), xs) = case startTokenType x of
    StartPunct punct ->
      pure (Just $ Punctuation lx punct, xs)
    StartSym -> do
      let ini = (lx.end, T.singleton x)
          inc (_, txt) (c, lc) = (lc.end, txt `T.snoc` c)
          fin (end', txt) = (lx{end = end'}, txt)
      ((l, sym) :> rest) <- S.fold inc ini fin $ S.span (contSymbol . fst) xs
      pure (Just $ Symbol l sym, rest)
    StartNum -> do
      -- TODO look for alternate bases
      let ini = (lx.end, [x])
          inc (_, revacc) (c, lc) = (lc.end, c:revacc)
          fin (end', revacc) = (lx{end = end'}, read $ reverse revacc)
      ((l, num) :> rest) <- S.fold inc ini fin $ S.span (contDigits . fst) xs
      -- TODO peek for a decimal point
      -- TODO peek for an exponent
      pure (Just $ IntLit l num, rest)
    StartWs -> pure (Nothing, S.dropWhile (isWs . fst) xs)
    StartComment -> pure (Nothing, S.dropWhile (not . isNewline . fst) xs)
    StartIll -> pure (Nothing, S.dropWhile (isIllegal . fst) xs)
  loop :: Maybe Token -> CharStream m () -> TokStream m ()
  loop tok rest = do
    maybe (pure ()) S.yield tok
    tokenize rest

------ Character Classification ------

data StartTokenType
  = StartPunct PunctToken
  | StartSym
  -- TODO Sign, for (`elem` "+-") and we'll need another char to disambiguate
  | StartNum
  | StartStr StrQuote
  | StartWs
  | StartComment
  | StartIll
  deriving (Eq)

startTokenType :: Char -> StartTokenType
startTokenType c
  -- punctuation
  | c == '(' = StartPunct OpenParen
  | c == ')' = StartPunct CloseParen
  -- symbols
  | 'a' <= c && c <= 'z' = StartSym
  | 'A' <= c && c <= 'Z' = StartSym
  | c == '_' = StartSym
  -- numbers
  | '0' <= c && c <= '9' = StartNum
  -- strings
  | c == '\"' = StartStr DblQuote
  | c == '`' = StartStr Backtick
  -- whitespace
  | isWs c = StartWs
  -- comments
  | c == '#' = StartComment
  | otherwise = StartIll

contSymbol :: Char -> Bool
contSymbol c
  | 'a' <= c && c <= 'z' = True
  | 'A' <= c && c <= 'Z' = True
  | '0' <= c && c <= '9' = True
  | c `T.elem` "_-" = True
  | otherwise = False

contDigits :: Char -> Bool
contDigits c = '0' <= c && c <= '9'

isWs :: Char -> Bool
isWs = (`T.elem` " \t\n\r")

isNewline :: Char -> Bool
isNewline = (`T.elem` "\n\r")

isIllegal :: Char -> Bool
isIllegal = (== StartIll) . startTokenType

-----------------
------ AST ------
-----------------

------ Language ------

data Ast -- DEBUG
  = Atom Atom
  | Combo Span [Ast] Span
  deriving (Show)

data Atom
  = SymAtom Span Text
  | IntAtom Span Integer
  -- | SciAtom -- TODO
  | StrAtom StringToken
  deriving (Show)

------ Errors ------

class Monad m => ReaderError m where
  extraCloseParen :: (PunctToken, Span) -- ^ token+location of open paren
                  -> m a
  missingCloseParen :: (PunctToken, Span) -- ^ token+location of open paren
                    -> Maybe Token -- ^ token found instead
                    -> m a

instance ReaderError IO where
  extraCloseParen (tok, l) =
    die $ "extra close paren " <> show tok <> " at " <> show l
  missingCloseParen (tok, l) m_found = do
    let foundMsg = maybe "" (\tok2 -> "\n    found " <> show tok2) m_found
    die $ "missing close paren, opened at " <> show tok <> " at " <> show l <> foundMsg

------ Algorithm ------

reader :: ReaderError m => TokStream m r -> Stream (Node m) m r
reader s0 = go s0 >>= \case
  Left done -> pure done
  Right (Punctuation l CloseParen, _) -> S.effect $ extraCloseParen (CloseParen, l)
  Right (other, _) -> error $ "internal error in reader? expecting close paren or end of stream, found: " <> show other
  where
  -- the idea is that `go` consumes the tokens to produce nodes, and terminates either
  -- at the end of the tokens (Left result), or when an unmatched close paren is encountered (right)
  go :: ReaderError m => TokStream m r -> Stream (Node m) m (Either r (Token, TokStream m r))
  go s = S.effect $ S.next s <&> \case
    Left done -> pure $ Left done
    Right (tok, rest) -> case tok of
      Punctuation open OpenParen -> S.wrap $ Tree open $ go rest >>= \case
        Right (Punctuation close CloseParen, after) -> pure (close :> go after)
        Left _ -> S.effect $ missingCloseParen (OpenParen, open) Nothing
        Right (other, _) -> error $ "internal error in reader? expecting close paren or end of stream, found: " <> show other
      Punctuation _ CloseParen -> pure $ Right (tok, rest)
      Symbol l txt -> S.wrap $ Leaf (SymAtom l txt) (go rest)
      IntLit l n -> S.wrap $ Leaf (IntAtom l n) (go rest)
      StrLit str -> S.wrap $ Leaf (StrAtom str) (go rest)

toConcreteSyntax :: ReaderError m => Stream (Node m) m r -> m (Of [Ast] r) -- DEBUG
toConcreteSyntax s = S.inspect s >>= \case
  Left done -> pure ([] :> done)
  Right (Leaf x rest) -> do
    siblings :> done <- toConcreteSyntax rest
    pure $ (Atom x : siblings) :> done
  Right (Tree open next) -> do
    children :> (close :> rest) <- toConcreteSyntax next
    siblings :> done <- toConcreteSyntax rest
    pure $ (Combo open children close : siblings) :> done

------ Tree Streaming ------

-- Supports streaming s-expression data structures much like `Of` supports streaming list-like data.

data Node m r
  = Leaf !Atom r
  -- for `Tree`, I modelled the type after the return type of `span`:
  -- the idea is that the tree gives a stream of children of this tree,
  -- and when it's done, it gives the stream of nodes _after_ this tree
  -- (ie. imagin visiting the nodes in pre-traversal order: the `r` paratemter of the first stream
  -- is a stream of nodes visted after this tree and all its descendants are visited).
  -- but actually, I also want the tokens that start/stop this Node
  -- and actually, when you inspect, you get a `Stream` in the `r` position anyway
  | Tree !Span !(Stream (Node m) m (Of Span r))

instance Monad m => Functor (Node m) where
  fmap f (Leaf atom x) = Leaf atom (f x)
  fmap f (Tree open s) = Tree open (fmap (\(close :> x) -> close :> f x) s)

printNodes :: forall m r. S.MonadIO m => Stream (Node m) m r -> m r
printNodes = loop ""
  where
  loop :: String -> Stream (Node m) m x -> m x
  loop prefix s = S.inspect s >>= \case
   Left r -> pure r
   Right more -> case more of
     Leaf x rest -> S.liftIO (putStrLn $ prefix <> show x) >> loop prefix rest
     Tree open after -> do
       S.liftIO $ putStrLn $ prefix <> "START Tree " <> show open
       (close :> rest) <- loop ("  " <> prefix) after
       S.liftIO $ putStrLn $ prefix <> "END Tree " <> show close
       loop prefix rest

---------------------
------ Helpers ------
---------------------

-- DELME
-- peek :: (Monad m) => Stream (Of a) m r -> m (Maybe a, Stream (Of a) m r)
-- peek s = S.next s >>= pure . \case
--   Left r -> (Nothing, pure r)
--   Right (x, xs) -> (Just x, S.cons x xs)

die :: MonadIO m => String -> m a
die msg = liftIO $ hPutStrLn stderr msg >> exitFailure

--------------------------
------ Untyped Lisp ------
--------------------------

------ Language ------

data U_Expr
  = U_Var Text
  | U_App U_Expr [U_Expr]
  | U_Lam [Text] U_Expr
  | U_Lit U_Lit
  deriving (Show)

data U_Lit
  = U_Int Integer
  | U_Str Text
  deriving (Show)

------ Parser ------

toUexpr :: Ast -> U_Expr
toUexpr (Atom (SymAtom _ x)) = U_Var x
toUexpr (Atom (IntAtom _ i)) = U_Lit (U_Int i)
toUexpr (Combo _ (Atom (SymAtom _ "call") : f : args) _) = U_App (toUexpr f) (toUexpr <$> args)
toUexpr (Combo _ [Atom (SymAtom _ "fn"), (Combo _ params _), body] _) = U_Lam (toParam <$> params) (toUexpr body)
  where
  toParam (Atom (SymAtom _ x)) = x

------ Values ------

data U_Val m
  = U_Fun (U_Fun m)
  | U_Const U_Const
  | U_Builtin ([U_Val m] -> m (U_Val m))

data U_Const
  = U_IntC Integer
  | U_StrC Text
  | U_UnitC
  deriving (Show)

instance Show (U_Val m) where
  show (U_Fun _) = "<function>"
  show (U_Const c) = show c
  show (U_Builtin _) = "<builtin>"

data U_Fun m = U_Function
  { staticEnv :: Map Text (U_Val m)
  , params :: [Text]
  , body :: U_Expr
  }
  deriving (Show)

------ Evaluator ------

class MonadIO m => U_Eval m where -- TODO instead of taking in all of IO, sandbox it with the actual methods I need
  u_unboundVariable :: Text -> m a

instance U_Eval IO where
  u_unboundVariable x = error $ "unbound variable " <> show x

type U_Env m = Map Text (U_Val m)

u_startEnv :: forall m. U_Eval m => Map Text (U_Val m)
u_startEnv = Map.fromList $ second U_Builtin <$> builtins
  where
  builtins =
    [ ("add", primAdd)
    , ("printLnInt", primPrintIntLn)
    ]
  primAdd [U_Const (U_IntC x), U_Const (U_IntC y)] = pure $ U_Const $ U_IntC (x + y)
  primPrintIntLn [U_Const (U_IntC i)] = do
    liftIO (print i)
    pure $ U_Const U_UnitC

ueval :: U_Eval m => U_Env m -> U_Expr -> m (U_Val m)
ueval env (U_Var x) = case Map.lookup x env of
  Nothing -> u_unboundVariable x
  Just v -> pure v
ueval env (U_App getF getArgs) = do
  f <- ueval env getF
  args <- mapM (ueval env) getArgs
  uapply f args
ueval staticEnv (U_Lam params body) = pure $ U_Fun $ U_Function
  { staticEnv
  , params
  , body
  }
ueval _ (U_Lit (U_Int i)) = pure $ U_Const (U_IntC i)
ueval _ (U_Lit (U_Str str)) = pure $ U_Const (U_StrC str)

uapply :: U_Eval m => U_Val m -> [U_Val m] -> m (U_Val m)
uapply (U_Fun f) args | length args == length f.params = do
  let callEnv = Map.fromList $ zip f.params args
      env' = callEnv `Map.union` f.staticEnv
  ueval env' f.body
uapply (U_Builtin impl) args = impl args

