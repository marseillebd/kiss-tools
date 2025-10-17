-- TODO I think I should just build it with a single digit and wordsize (configured at the top of source)
-- cross-compiling is complexity that you don't want to hand-compile+assemble+link

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

{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude hiding (head)

import Data.Char (toLower)

head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

-- Entry Point
-- ===========

main :: IO ()
main = print "TODO"

-- File Options
-- ============

parseMeta :: Line -> Either ParseError (FileMeta -> FileMeta)
parseMeta line = do
  (name, value) <- parseMetaLine
  case name of
    "digit" -> parseMetaDigit value
    "wordbits" -> undefined
    "defaultformat" -> undefined
    _ -> Left $ BadOptionName name
  where
  parseMetaLine :: Either ParseError (String, String)
  parseMetaLine = do
    case splitSimpleId line of
      Just (name, ':':value) -> pure (toLower <$> name, dropWs value)
      _ -> Left $ BadOptionLine line

parseMetaDigit :: String -> Either ParseError (FileMeta -> FileMeta)
parseMetaDigit value = case toLower <$> value of
  "hex" -> Right $ update 4
  "oct" -> Right $ update 3
  "bin" -> Right $ update 1
  _ -> Left $ BadOptionValue "digit" value
  where
  update n opts = opts{digit = n:opts.digit}

-- TODO parseMetaWordbits

-- TODO parseMetaDefaultFormat

-- TODO validate the parsed options

-- Support
-- =======

type Line = String

type Name = String

-- Configuration
-- -------------

-- TODO a way to set the output's words per line?
-- or, just keep the whitespace as it exists
-- probably would just be a CLI flag, since output format isn't tied to the input files

data FileMeta = FileMeta
  { digit :: [Int] -- ^ number of bits per digit
  , word :: [Int] -- ^ number of bits per word
  }

emptyFileMeta :: FileMeta
emptyFileMeta = FileMeta
  { digit = []
  , word = []
  -- , defaultFormat = [] -- TODO actually, the default format should likely be part of the data stream
  }

-- Parsing
-- -------

dropWs :: String -> String
dropWs = dropWhile (`elem` " \t")

inClass :: Char -> String -> Bool
inClass c = go
  where
  go (a:'-':b:rest) = (a <= c && c <= b) || go rest
  go (a:rest) = c == a || go rest
  go [] = False

splitSimpleId :: String -> Maybe (String, String)
splitSimpleId [] = Nothing
splitSimpleId (c:tl)
  | c `inClass` "a-zA-Z"
  , (cs, rest) <- span (`inClass` "a-zA-Z0-9_") tl
  = Just (c:cs, rest)
  | otherwise = Nothing

-- Errors
-- ------

data ParseError
  = BadOptionLine Line
  | BadOptionName Name
  | BadOptionValue Name String
  deriving(Show)
