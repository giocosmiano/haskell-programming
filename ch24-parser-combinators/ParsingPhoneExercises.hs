{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingPhoneExercises where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- parser for US/Canada phone numbers with varying formats
-----------------------------------------------------------------------------------

type NumberingPlanArea = Integer
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                 deriving (Eq, Show)

-----------------------------------------------------------------------------------

-- e.g.
-- parseString parsePhone mempty "123-456-7890"   -> Success (PhoneNumber 123 456 7890)
-- parseString parsePhone mempty "1234567890"     -> Success (PhoneNumber 123 456 7890)
-- parseString parsePhone mempty "(123) 456-7890" -> Success (PhoneNumber 123 456 7890)
-- parseString parsePhone mempty "1-123-456-7890" -> Success (PhoneNumber 123 456 7890)
parsePhone :: Parser PhoneNumber
parsePhone = do
  v <- parsePhoneDigit
  let list  = foldr (\x b -> show x ++ b) [] v
      npa   = take 3 list
      nxx   = take 3 $ drop 3 list
      lnNbr = take 4 $ drop 6 list
  return $ PhoneNumber (read npa) (read nxx) (read lnNbr)

parsePhoneDigit :: Parser [Integer]
parsePhoneDigit = some $ do
--  _ <- skipMany (string "1-")
--  _ <- skipMany (oneOf "- ()")
--  v <- some digit
  v <- many (string "1-") >> many (oneOf "- ()") >> some digit
  return (read v)

-----------------------------------------------------------------------------------

-- e.g.
-- parseString parsePhone' mempty "123-456-7890"   -> Success (PhoneNumber 123 456 7890)
-- parseString parsePhone' mempty "1234567890"     -> Success (PhoneNumber 123 456 7890)
-- parseString parsePhone' mempty "(123) 456-7890" -> Success (PhoneNumber 123 456 7890)
-- parseString parsePhone' mempty "1-123-456-7890" -> Success (PhoneNumber 123 456 7890)
parsePhone' :: Parser PhoneNumber
parsePhone' = do
  optional (string "1-")

  parsePhoneDelim
  npa <- count 3 digit
  parsePhoneDelim

  parsePhoneDelim
  nxx <- count 3 digit

  parsePhoneDelim
  lnNbr <- count 4 digit

  return $ PhoneNumber (read npa) (read nxx) (read lnNbr)

parsePhoneDelim :: Parser (Maybe Char)
parsePhoneDelim = optional (oneOf "- ()")

-----------------------------------------------------------------------------------
