module Main (main) where

import Control.Monad (when)
import Data.Char (ord, chr)
import System.IO (stdout, stderr, putChar, hPutStrLn, hSetBinaryMode)
import System.Exit (exitFailure)

main :: IO ()
main = do
  hSetBinaryMode stdout True
  getContents >>= mainLoop 0
  where
  mainLoop :: Int -> String -> IO ()
  mainLoop _ [] = pure ()
  mainLoop !i (c:rest)
    | c `elem` " _" = mainLoop i rest
    | c =='(' = mainLoop i (skipParens rest)
    | c `elem` "#;" = mainLoop i (skipLine rest)
    | c == '@'
    , Just (addr, rest2) <- fromHexAddr rest = do
      when (i /= addr) $ die $ unwords
        [ "address assertion failed:"
        , "expected", show addr, "at offset", show i
        ]
      mainLoop i (skipLine rest2)
    | Just hi <- fromHexDigit c
    , c2:rest2 <- rest
    , Just lo <- fromHexDigit c2
    = do
      putChar (chr $ hi*16 + lo)
      mainLoop (i + 1) rest2
    | otherwise = die "syntax error"
  skipParens = drop 1 . dropWhile (/= ')')
  skipLine = drop 1 . dropWhile (/= '\n')

fromHexDigit :: Char -> Maybe Int
fromHexDigit c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | otherwise = Nothing

fromHexAddr :: String -> Maybe (Int, String)
fromHexAddr str0 = if null str0 then Nothing else loop 0 str0
  where
  loop :: Int -> String -> Maybe (Int, String)
  loop addr str = case uncons str of
    Just (c, cs)
      | Just nybble <- fromHexDigit c ->
        loop (addr * 16 + nybble) cs
      | c == '_' -> loop addr cs
      | c `elem` " \n" -> Just (addr, str)
    Nothing -> Just (addr, str)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (c:cs) = Just (c, cs)

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure
