{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module MarshallingJSON where

import Control.Applicative

import qualified Data.ByteString as STRICT
import qualified Data.ByteString.Lazy as LAZY

import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

--
-- Fast JSON parsing and encoding
-- https://github.com/bos/aeson
--
-- Prelude> stack install aeson
-- http://hackage.haskell.org/package/aeson
import Data.Aeson

-----------------------------------------------------------------------------------

sectionJsonStrict :: STRICT.ByteString
sectionJsonStrict = [r|
{ "section":  {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

sectionJsonLazy :: LAZY.ByteString
sectionJsonLazy = [r|
{ "section":  {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

-----------------------------------------------------------------------------------
-- unmarshalling json into `yourType`
-----------------------------------------------------------------------------------

data TestData = TestData {
                section :: Host
              , what :: Color
              } deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color = Red Annotation
           | Blue Annotation
           | Yellow Annotation
           deriving (Eq, Show)

-----------------------------------------------------------------------------------

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
    (Red <$> v .: "red") <|> (Blue <$> v .: "blue") <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected an object for Color"

-----------------------------------------------------------------------------------
-- json marshalling and unmarshalling
-----------------------------------------------------------------------------------

-- FromJSON
-- ByteString -> Value -> yourType
-- parse -> unmarshall

-- ToJSON
-- yourType -> Value -> ByteString
-- marshall -> serialize

-- | A JSON value represented
-- as a Haskell value.
-- data Value = Object !Object
--            | Array !Array
--            | String !Text
--            | Number !Scientific
--            | Bool !Bool
--            | Null
--            deriving (Eq, Read, Show, Typeable, Data)

-----------------------------------------------------------------------------------
-- unmarshalling a string or number
-----------------------------------------------------------------------------------

data NumberOrString = Numba Integer
                    | Stringy Text
                    deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number"
      (Right integer) -> return $ Numba integer

  parseJSON (String s) = return $ Stringy s
  parseJSON _ =
    fail "NumberOrString must\
         \ be number or string"

-- so it knows what we want to parse
dec :: LAZY.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LAZY.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

-----------------------------------------------------------------------------------

main = do
  let blah :: Maybe Value
      blah = decodeStrict sectionJsonStrict
  print blah

  let blahLazy :: Maybe Value
      blahLazy = decode sectionJsonLazy
  print blahLazy

  let jsonStrict :: Maybe TestData
      jsonStrict = decodeStrict sectionJsonStrict
  print jsonStrict

  let jsonLazy :: Maybe TestData
      jsonLazy = decode sectionJsonLazy
  print jsonLazy

-----------------------------------------------------------------------------------
