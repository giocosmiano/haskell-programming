module VigenereCipherExercises where

import Data.List
import Data.Char (chr, ord, isUpper, isLower)
import System.Environment
import System.Exit
import System.IO

-- System.Environment.getArgs :: IO [String]
-- System.IO.hPutStr :: Handle -> String -> IO ()
-- System.IO.hGetChar :: Handle -> IO Char
-- System.IO.stdout :: Handle
-- System.IO.stdin :: Handle
-- System.IO.hWaitForInput :: Handle -> Int -> IO Bool
-- System.IO.stderr :: Handle

-----------------------------------------------------------------------------------

-- get the decimal value of the character
-- https://en.wikipedia.org/wiki/List_of_Unicode_characters
getDecOfA :: Char -> Int
getDecOfA x
   | isUpper x = 65 -- A=65
   | isLower x = 97 -- a=97
   | otherwise = 0

charToDec :: Char -> Int
charToDec x = ord x - getDecOfA x

shift :: Char -> Char -> Char
shift key str
   | isLower str || isUpper str = chr $ newDec + getDecOfA str
   | otherwise  = str
   where decKey = charToDec key
         decStr = charToDec str
         newDec = mod (decKey + decStr) 26

-- e.g. key       = "ALLY"
--    , plainText = "MEET AT DAWN"
--   -> resultKey = "ALLY AL LYAL"
-- replaceKeyChar :: key -> plainText -> resultKey
replaceKeyChar :: String -> String -> String
replaceKeyChar [] _ = []
replaceKeyChar _ [] = []
replaceKeyChar key@(x:xs) (y:ys)
   | isLower y || isUpper y = x : replaceKeyChar xs  ys
   | otherwise              = y : replaceKeyChar key ys

-- e.g.
--    , key       = "ALLY"
--    , plainText = "MEET AT DAWN"
--   -> resultKey = "ALLY AL LYAL"
-- mapKey "ALLY" "MEET AT DAWN" -> "ALLY AL LYAL"
-- mapKey :: key -> plainText -> resultKey
mapKey :: String -> String -> String
mapKey key str   =
   let nbrCopies = quot (length str) (length key)
       dupedKeys = concat $ replicate (nbrCopies + 1) key
   in  replaceKeyChar dupedKeys str

-- Vigenere ciphering only the 26-letters of the alphabet
-- see https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher
-- cipher :: key -> plainText -> cipherText
cipher :: String -> String -> String
cipher  _ [] = []
cipher []  _ = []
cipher key str =
   let keyStr    = mapKey key str
       pairStr   = zip keyStr str
   in  map (\(k, s) -> shift k s) pairStr

-- e.g.
-- cipher "ALLY" "MEET AT DAWN" -> "MPPR AE OYWY"
--    , key        = "ALLY"
--    , plainText  = "MEET AT DAWN"
--   -> cipherText = "MPPR AE OYWY"

-- cipher "ALLY" "Meet At Dawn" -> "Mppr Ae Oywy"
--    , key        = "ALLY"
--    , plainText  = "Meet At Dawn"
--   -> cipherText = "Mppr Ae Oywy"

-----------------------------------------------------------------------------------

data Args = Args {
    mode :: String
  , key  :: String
  } deriving (Show, Eq)

tac  = unlines . reverse . lines

mapArgs :: [String] -> IO Args
mapArgs (mode: key: _) =
  case mode of
    "-d" -> return $ Args "decrypt" key
    "-e" -> return $ Args "encrypt" key
    _    -> usage >> exit
mapArgs _ = usage >> exit

parse :: [String] -> IO Args
parse ["-v"] = version >> exit
parse ["-h"] = usage   >> exit
parse []     = usage   >> exit
parse args   = mapArgs args

--parse fs     = concat `fmap` mapM readFile fs

usage :: IO ()
usage = do
  putStrLn "Usage: VigenereCipherExercises [mode] [key]"
  putStrLn "    mode --> -d to decrypt, -e to encrypt"
  putStrLn "    key  --> key string to encrypt/decrypt"

version :: IO ()
version = putStrLn "Haskell VigenereCipherExercises 0.1"

exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs >>= parse
  str  <- getLine

  putStrLn . show $ args
  putStrLn str

  case mode args of
    "encrypt" -> putStrLn $ cipher (key args) str
    "decrypt" -> putStrLn str

-----------------------------------------------------------------------------------




