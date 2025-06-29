{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Streaming.Prelude (Stream, Of(..))

import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  S.print $ tokenize $ charSpans Nothing (Off 1 1) $ S.each "(a b (1 2 3) c)"
  putStrLn ""
  printNodes $ goes $ tokenize $ charSpans Nothing (Off 1 1) $ S.each "(a b (1 2 3) c)"
  putStrLn ""
  (print =<<) $ blastme $ tokenize $ charSpans Nothing (Off 1 1) $ S.each "(a b (1 2 3) c)"

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
  | c `elem` "_-" = True
  | otherwise = False

contDigits :: Char -> Bool
contDigits c = '0' <= c && c <= '9'

isWs :: Char -> Bool
isWs = (`elem` " \t\n\r")

isNewline :: Char -> Bool
isNewline = (`elem` "\n\r")

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

------ Algorithm ------

goes :: Monad m => TokStream m r -> Stream (Node m) m r
goes s = go s <&> \case
  Left done -> done
  Right (close@(Punctuation _ CloseParen), _) -> error $ "extra close paren: " <> show close
  Right (other, _) -> error $ "internal error in goes? expecting close paren or end of stream, found: " <> show other
-- the idea is that `go` consumes the tokens to produce nodes, and terminates either
-- at the end of the tokens (Left result), or when an unmatched close paren is encountered (right)
go :: Monad m => TokStream m r -> Stream (Node m) m (Either r (Token, TokStream m r))
go s = S.effect $ S.next s <&> \case
  Left done -> pure $ Left done
  Right (tok, rest) -> case tok of
    Punctuation open OpenParen -> S.wrap $ Tree open $ go rest <&> \case
      Right (Punctuation close CloseParen, after) -> (close :> go after)
      Left _ -> error $ "missing close paren"
      Right (other, _) -> error $ "internal error in go? expecting close paren or end of stream, found: " <> show other
    Punctuation _ CloseParen -> pure $ Right (tok, rest)
    Symbol l txt -> S.wrap $ Leaf (SymAtom l txt) (go rest)
    IntLit l n -> S.wrap $ Leaf (IntAtom l n) (go rest)
    StrLit str -> S.wrap $ Leaf (StrAtom str) (go rest)

huh :: Monad m => Stream (Node m) m r -> m (Of [Ast] r) -- DEBUG
huh s = S.inspect s >>= \case
  Left done -> pure ([] :> done)
  Right (Leaf x rest) -> do
    siblings :> done <- huh rest
    pure $ (Atom x : siblings) :> done
  Right (Tree open next) -> do
    children :> (close :> rest) <- huh next
    siblings :> done <- huh rest
    pure $ (Combo open children close : siblings) :> done

blastme :: Monad m => TokStream m () -> m [Ast]
blastme s = do
  (ast :> ()) <- huh (goes s)
  pure ast

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

-------------------------------
------ Streaming Helpers ------
-------------------------------

-- DELME
-- peek :: (Monad m) => Stream (Of a) m r -> m (Maybe a, Stream (Of a) m r)
-- peek s = S.next s >>= pure . \case
--   Left r -> (Nothing, pure r)
--   Right (x, xs) -> (Just x, S.cons x xs)
