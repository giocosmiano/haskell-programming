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

-- |
-- get the decimal value of the character
-- https://en.wikipedia.org/wiki/List_of_Unicode_characters
getDecOfA :: Char -> Int
getDecOfA x
   | isUpper x = 65 -- A=65
   | isLower x = 97 -- a=97
   | otherwise = 0

charToDec :: Char -> Int
charToDec x = ord x - getDecOfA x

-----------------------------------------------------------------------------------

-- |
-- Vigenere ciphering only the 26-letters of the alphabet
-- https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher
shift :: (Int -> Int -> Int) -> Char -> Char -> Char
shift f key str
   | isLower str || isUpper str = chr $ newDec + getDecOfA str
   | otherwise  = str
   where decKey = charToDec key
         decStr = charToDec str
         newDec = mod (decStr `f` decKey) 26

-- |
-- shift :: Char -> Char -> Char
-- shift key str
--    | isLower str || isUpper str = chr $ newDec + getDecOfA str
--    | otherwise  = str
--    where decKey = charToDec key
--          decStr = charToDec str
--          newDec = mod (decStr + decKey) 26
--
-- unshift :: Char -> Char -> Char
-- unshift key str
--    | isLower str || isUpper str = chr $ newDec + getDecOfA str
--    | otherwise  = str
--    where decKey = charToDec key
--          decStr = charToDec str
--          newDec = mod (decStr - decKey) 26

-----------------------------------------------------------------------------------

-- |
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

-----------------------------------------------------------------------------------

-- |
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

-----------------------------------------------------------------------------------

-- |
-- Vigenere ciphering only the 26-letters of the alphabet
-- https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher
-- cipher :: (+) -> key -> plainText  -> cipherText
-- cipher :: (-) -> key -> cipherText -> plainText
cipher :: (Int -> Int -> Int) -> String -> String -> String
cipher f _ [] = []
cipher f [] _ = []
cipher f key str =
   let keyStr    = mapKey key str
       pairStr   = zip keyStr str
   in  map (\(k, s) -> shift f k s) pairStr

-- |
-- e.g.
-- cipher (+) "ALLY" "MEET AT DAWN" -> "MPPR AE OYWY"
--    , key        = "ALLY"
--    , plainText  = "MEET AT DAWN"
--   -> cipherText = "MPPR AE OYWY"
--
-- cipher (+) "ALLY" "Meet At Dawn" -> "Mppr Ae Oywy"
--    , key        = "ALLY"
--    , plainText  = "Meet At Dawn"
--   -> cipherText = "Mppr Ae Oywy"
--
-- cipher (-) "ALLY" "MPPR AE OYWY" -> "MEET AT DAWN"
--    , key        = "ALLY"
--    , cipherText = "MPPR AE OYWY"
--   -> plainText  = "MEET AT DAWN"
--
-- cipher (-) "ALLY" "Mppr Ae Oywy" -> "Meet At Dawn"
--    , key        = "ALLY"
--    , cipherText = "Mppr Ae Oywy"
--   -> plainText  = "Meet At Dawn"

-----------------------------------------------------------------------------------

data Args = Args {
    mode :: String
  , key  :: String
  , file :: Maybe String
  } deriving (Show, Eq)

tac  = unlines . reverse . lines

-----------------------------------------------------------------------------------

argMode :: String -> IO String
argMode [] = usage >> exit
argMode xs =
  case xs of
    "-d" -> return "decrypt"
    "-e" -> return "encrypt"
    _    -> usage >> exit

argKey :: String -> IO String
argKey [] = usage >> exit
argKey xs = return xs

argFile :: String -> IO (Maybe String)
argFile [] = return Nothing
argFile xs = return $ Just xs

mapArgs :: [String] -> IO Args
mapArgs (mode : key : file : _) = do
  mode' <- argMode mode
  key'  <- argKey  key
  file' <- argFile file
  return $ Args mode' key' file'
mapArgs (mode : key : _) = do
  mode' <- argMode mode
  key'  <- argKey  key
  return $ Args mode' key' Nothing
mapArgs _ = usage >> exit

-----------------------------------------------------------------------------------

parse :: [String] -> IO Args
parse ["-v"] = version >> exit
parse ["-h"] = usage   >> exit
parse []     = usage   >> exit
parse args   = mapArgs args

--parse fs     = concat `fmap` mapM readFile fs

-----------------------------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn "Usage: VigenereCipherExercises [mode] [key] [file]"
  putStrLn "    mode --> -e to encrypt, -d to decrypt (required)"
  putStrLn "    key  --> encryption/decryption key    (required)"
  putStrLn "    file --> file to encrypt/decrypt      (optional)"

version :: IO ()
version = putStrLn "Haskell VigenereCipherExercises 0.1"

exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs >>= parse
  str  <- getLine

  putStrLn . show $ args
  putStrLn str

  case mode args of
    "encrypt" -> putStrLn $ cipher (+) (key args) str
    "decrypt" -> putStrLn $ cipher (-) (key args) str

-- |
-- $ stack VigenereCipherExercises.hs  "-e" "ALLY"
-- Meet At Dawn
-- Args {mode = "encrypt", key = "ALLY"}
-- Meet At Dawn
-- Mppr Ae Oywy
--
-- $ stack VigenereCipherExercises.hs  "-d" "ALLY"
-- Mppr Ae Oywy
-- Args {mode = "decrypt", key = "ALLY"}
-- Mppr Ae Oywy
-- Meet At Dawn

-----------------------------------------------------------------------------------




