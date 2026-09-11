{-# LANGUAGE OverloadedStrings #-}

module HexObject where

import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Word (Word8)
import Numeric (showHex)
import Data.Text (Text)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.Text as T

-- $thePlan
--
-- We can reasonably break up the representation of a "binary" file (like ELF) into different levels.
-- - just the bytes
-- - add in symbol definitions and patches (relocation info)
-- - add in origin, alignment, padding
-- - add in uninitialized/zero'd reservations
--
-- Ok, so these are not strictly levels, but related bits of functionality


---------------------
-- * Bytes Records --
---------------------

-- | a typed tagless final constructor for unadulterated bytes
class Bytes b where
  bytes :: ByteString -> b
-- | the initial data type for Bytes
instance Bytes ByteString where bytes = id

-- | encode bytes into human-readable text
instance Bytes Text where
  bytes bs = addQuotes $ T.concat $ map doByte $ BS.unpack bs
    where
    addQuotes x = "\"" <> x <> "\""
    -- TODO: perhaps add underscores every four hexdigits
    doByte :: Word8 -> Text
    doByte b
      -- TODO: perhaps special escapes, like C-escapes?
      -- standard encoding
      | b == 0x22 -- double-quote
      || b == 0x5C -- backslash
      || b < 0x20 || b >= 0x7F -- outside ascii printing
        = "\\x" <>
          (if b < 0x10 then "0" else "") <>
          (T.pack $ showHex b "")
      | otherwise = T.singleton $ chr (fromIntegral b)
-- | decode human-readable text into bytes
readBytes :: Bytes bytes => String -> Maybe (bytes, String)
readBytes input = case span isBytey input of
  ("", _) -> Nothing
  (byteyChars, rest) -> do
    let hex = filter isHex byteyChars
    bs <- BS.pack <$> go (filter isHex byteyChars)
    pure (bytes bs, rest)
  where
  isBytey b = isHex b || b == '_'
  isHex b =  ('0' <= b && b <= '0')
          || ('a' <= b && b <= 'f')
          || ('A' <= b && b <= 'F')
  -- this is internal because it relies on the assumptions enforced by `filter isHex`
  go :: String -> Maybe [Word8]
  go (hi:lo:rest) = do
    (:) <$> readMaybe ['0','x', hi, lo] <*> go rest
  go [] = Just []
  go [_] = Nothing

----------------------
-- * Offset Records --
----------------------

-- TODO: `=<hex>` syntax to assert the value of this offset
-- TODO: `.padz <num bytes>
-- TODO: `.alignz <pow2 num bytes>
-- NOTE: I've got the `z` suffix on pad and align to leave open the chance for padding with a non-zero pattern

--------------------------
-- * Relocation Records --
--------------------------

-- TODO: `@<ident>` syntax to define a symbol as this offset

-----------------------
-- * Segment Records --
-----------------------

-- TODO: `.org <n>` to set origin in memory where following bytes will be copied
-- TODO: `.undefz <n>` to reserve bytes in memory (zero'd out, same reasoning for z as in padz)

-- NOTE: pad and align are just there so that they can generate real zero bytes when the human form is lowered to binary
-- meanwhile, undef emits an instruction to the loader
-- likewize, .org is an instruction for the loader
-- TODO: so maybe I should use !org, !undefz for executable files and .<directive> for linker files
-- heck, JCL uses \\ to do _something_ executable related, and maybe that's good too
-- the point is, it should be clear when a directive is for the ahead-of-time linker vs the loader/dynamic linker
