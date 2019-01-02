module LearnParsers where

--
-- A modern parser combinator library with convenient diagnostics
-- https://github.com/ekmett/trifecta/
--
-- Prelude> stack install trifecta
-- https://hackage.haskell.org/package/trifecta-1.5.2
import Text.Trifecta

-----------------------------------------------------------------------------------

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1', then die
one' :: Parser b
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1', and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

-----------------------------------------------------------------------------------

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-----------------------------------------------------------------------------------

