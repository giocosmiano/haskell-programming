module VigenereCipherExercises where

import Control.DeepSeq
import Data.Maybe
import Data.Char (chr, ord, isUpper, isLower, isAlpha)
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
-- Vigenere ciphering/de-ciphering algorithm
-- https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher?section=3#Algebraic_description
shift :: (Int -> Int -> Int) -> Char -> Char -> Char
shift f key str
   | isAlpha str = chr $ newDec + getDecOfA str
   | otherwise  = str
   where decKey = charToDec key
         decStr = charToDec str
         newDec = mod (decStr `f` decKey) 26

-- |
-- shift :: Char -> Char -> Char
-- shift key str
--    | isAlpha str = chr $ newDec + getDecOfA str
--    | otherwise  = str
--    where decKey = charToDec key
--          decStr = charToDec str
--          newDec = mod (decStr + decKey) 26
--
-- unshift :: Char -> Char -> Char
-- unshift key str
--    | isAlpha str = chr $ newDec + getDecOfA str
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
   | isAlpha y = x : replaceKeyChar xs  ys
   | otherwise = y : replaceKeyChar key ys

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
    mode    :: String
  , key     :: String
  , inFile  :: Maybe String
  , outFile :: Maybe String
  } deriving (Show, Eq)

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

-----------------------------------------------------------------------------------

mapArgs :: [String] -> IO Args
mapArgs (mode : key : inFile : outFile : _) = do
  mode'    <- argMode mode
  key'     <- argKey  key
  inFile'  <- argFile inFile
  outFile' <- argFile outFile
  return $ Args mode' key' inFile' outFile'

mapArgs (mode : key : inFile : _) = do
  mode'    <- argMode mode
  key'     <- argKey  key
  inFile'  <- argFile inFile
  return $ Args mode' key' inFile' Nothing

mapArgs (mode : key : _) = do
  mode' <- argMode mode
  key'  <- argKey  key
  return $ Args mode' key' Nothing Nothing

mapArgs _ = usage >> exit

-----------------------------------------------------------------------------------

parseArgs :: [String] -> IO Args
parseArgs ["-v"] = version >> exit
parseArgs ["-h"] = usage   >> exit
parseArgs []     = usage   >> exit
parseArgs args   = mapArgs args

-----------------------------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn "Usage: VigenereCipherExercises [mode] [key] [inFile] [outFile]"
  putStrLn "    mode    --> -e to encrypt, -d to decrypt    (required)"
  putStrLn "    key     --> encryption/decryption key       (required)"
  putStrLn "    inFile  --> input file to encrypt/decrypt   (optional)"
  putStrLn "    outFile --> encrypted/decrypted output file (optional)"
  putStrLn "examples..."
  putStrLn "    $ stack VigenereCipherExercises.hs -e ALLY"
  putStrLn "    $ stack VigenereCipherExercises.hs -d ALLY"
  putStrLn "    $ stack VigenereCipherExercises.hs -e ALLY VigenereCipherExercises.hs          VigenereCipherExercises.encrypt.log"
  putStrLn "    $ stack VigenereCipherExercises.hs -d ALLY VigenereCipherExercises.encrypt.log VigenereCipherExercises.decrypt.log"

version :: IO ()
version = putStrLn "Haskell VigenereCipherExercises 0.1"

exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)

-----------------------------------------------------------------------------------

-- |
-- http://learnyouahaskell.com/input-and-output
--
-- withFile vs. openFile issue
-- https://stackoverflow.com/questions/9406463/withfile-vs-openfile
-- https://stackoverflow.com/questions/26949378/what-caused-this-delayed-read-on-closed-handle-error
-- https://self-learning-java-tutorial.blogspot.com/2016/05/haskell-withfile-perform-operation-on.html
--
-- Prelude> :t ($!!)
-- ($!!) :: NFData a => (a -> b) -> a -> b

fileHandler :: Handle -> IO String
fileHandler handle = do
  hSetBuffering handle $ BlockBuffering (Just 2048)
  contents <- hGetContents handle
--   putStr contents -- no need to print `contents` to resolve error "delayed read on closed handle"
  return $!! contents -- forcing the string to "normal form" using ($!!) from Control.DeepSeq

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs >>= parseArgs

  putStrLn . show $ args

  str <-
    case inFile args of
      Nothing -> getLine
      Just fs -> withFile fs ReadMode fileHandler
--       Nothing -> do
--         str <- getLine
--         return str
--       Just fs -> do
--         str <- withFile fs ReadMode fileHandler
--         return str

-- |
-- Vigenere ciphering/de-ciphering algorithm
-- https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher?section=3#Algebraic_description
  let func = if mode args == "encrypt" then (+) else (-)
      cipherTxt = cipher func (key args) str

  putStrLn cipherTxt

  case outFile args of
    Nothing -> return ()
    Just fs -> do
      writeFile fs cipherTxt
      return ()

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
--
-- $ stack VigenereCipherExercises.hs -e ALLY VigenereCipherExercises.hs           VigenereCipherExercises.encrypt.log
--
-- $ stack VigenereCipherExercises.hs -d ALLY VigenereCipherExercises.encrypt.log  VigenereCipherExercises.decrypt.log

-----------------------------------------------------------------------------------




