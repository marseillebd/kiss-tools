-- TODO the think I'm working on seems more like a linker than an assembler, tbh

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

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Prelude hiding (head, words)

import Control.Monad (forM_)
import Data.Bits (Bits(shiftL))
import Data.Char (ord, toLower)
import System.Exit (exitFailure)
import System.IO (getContents)

head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

forAccum_ :: (Monad m) => b -> [a] -> (b -> a -> m b) -> m b
forAccum_ st0 xs0 f = go st0 xs0
  where
  go acc [] = pure acc
  go acc (x:xs) = do
    acc' <- f acc x
    go acc' xs

die :: (Show msg) => msg -> IO a
die msg = print msg >> exitFailure -- TODO should print to stderr

-- Hardcoded Configuration
-- =======================

-- Keep it simple!
-- Why write and compile a bunch of code to manage option parsing and validation when the user is expected to be able to hand-compile from source?
-- I've written this for 8-bit addressible data encoded in hexadecimal.
-- If you need a different format, these are easy function/values to recode.

-- Should specify number of bits per word.
-- A word defined as filling the bitspace between two addresses.
-- On most modern machines, this is eight bits, but you might need to target oddball machines.
-- I'm personally partial to the "9-bit byte", so if I get a simulator for such a machine, this is the number I'd change to compensate.
bitsPerWord :: Int
bitsPerWord = 8

-- Should (effectively) specify the base used to encode words in text (in the source).
-- For exampe, a hexidecimal digit can hold four bits, whereas an octal digit holds three.
-- If you need a non-integral number of bits per digit (e.g. decimal), delete this value and modify `digitsPerWord` and `spanWord` specially.
bitsPerDigit :: Int
bitsPerDigit = 4

splitDigit :: String -> Maybe (Int, String)
splitDigit (c:rest)
  | '0' <= c && c <= '9' = pure (ord c - ord '0', rest)
  | 'A' <= c && c <= 'F' = pure (ord c - ord 'A' + 10, rest)
  | otherwise = Nothing
splitDigit "" = Nothing

-- Adapt Configuration to Functions
-- --------------------------------
--
-- Instead of using the "config" directly, we wrap them into slightly-higher-level operations for use in the rest of the program.
-- Effectively, these make it easier to swap out some of the implementation.

digitsPerWord :: Int
digitsPerWord = case bitsPerWord `divMod` bitsPerDigit of
  (n, 0) -> n
  (n, _) -> n + 1

splitWord :: String -> Maybe (Int, String)
splitWord = go digitsPerWord 0
  where
  go 0 acc rest = pure (acc, rest)
  go i acc str = do
    (digitVal, rest) <- splitDigit str
    go (i - 1) (shiftL acc bitsPerDigit + digitVal) rest

-- Entry Point
-- ===========

main :: IO ()
main = do
  input <- getContents
  accum <- case parseMany emptyAccum input parseToken of
    Right revacc -> pure $ revacc
      { body = reverse revacc.body
      }
    Left err -> die err
  -- TODO everything after this is hacky test stuff
  let compressNewlines ('\n':'\n':rest) = compressNewlines ('\n':rest)
      compressNewlines (c:rest) = c : compressNewlines rest
      compressNewlines [] = []
  let displayText :: BodyElem -> String
      displayText (Word n) = show n -- TODO show in hex (or rather, use config'd render func
      displayText (Ws ws) = ws
  putStrLn "LABELS:"
  forM_ accum.labels $ \(name, offset) -> putStrLn $ name ++ ": " ++ show offset
  putStrLn "BODY:"
  putStr $ compressNewlines (concat $ displayText <$> accum.body)

-- TODO do I read the body into memory or write it to a file
-- (and then patch before writing anything to disk, or write as I go and use fseek to patch?)
-- for now, I'll write to memory, because that's easier

data Accum = Accum
  { offset :: !Offset
  , body :: [BodyElem]
  , labels :: [(Name, Offset)]
  }
  deriving (Show)

emptyAccum :: Accum
emptyAccum = Accum
  { offset = 0
  , body = []
  , labels = []
  }

data BodyElem
  = Word Int
  | Ws String
  deriving(Show)

parseToken :: Accum -> String -> Either LinkError (Accum, String)
parseToken acc input = if
  -- plain word
  | Just (word, rest) <- splitWord input
  -> let acc' = acc
          { offset = acc.offset + 1
          , body = Word word:acc.body
          }
     in  pure (acc', rest)
  -- labels
  | Just (name, input') <- splitId input
  , (':':rest) <- input'
  -> pure (acc{labels = (name, acc.offset):acc.labels}, rest)
  -- TODO patch point
  -- TODO directives (pad, align, default layout, let & maybe var, namespace, define patch layout)
  | (':':rest0) <- input
  , Just (name, rest) <- splitSimpleId rest0
  -> undefined
  -- comments
  | ('#':_) <- input
  -> pure $ (acc{body = Ws input:acc.body}, "")
  -- whitespace and end of line
  | "" <- input
  -> pure (acc{body = Ws "\n":acc.body}, "")
  | Just (ws, rest) <- splitWs input
  -> pure (acc{body = Ws ws:acc.body}, rest)
  -- syntax error
  | otherwise
  -> Left $ SyntaxError input

parseMany :: s -> String -> (s -> String -> Either LinkError (s, String)) -> Either LinkError s
parseMany st0 str0 f = go st0 str0
  where
  go st "" = pure st
  go st str = do
    (st', rest) <- f st str
    go st' rest

-- Support
-- =======

type Name = String -- TODO should be a newtype

type Offset = Int -- TODO should be unsigned, and perhaps a newtype

-- Parsing
-- -------

splitWs :: String -> Maybe (String, String)
splitWs "" = Nothing
splitWs it = Just $ span (`elem` " \t\n\r") it

inClass :: Char -> String -> Bool
inClass c = go
  where
  go (a:'-':b:rest) = (a <= c && c <= b) || go rest
  go (a:rest) = c == a || go rest
  go [] = False

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

simpleIdStart = "a-zA-Z"
idStart = simpleIdStart ++ "@$^_."
idBody = (++ "0-9")

-- Errors
-- ------

data LinkError
  = SyntaxError String
  deriving(Show)
