{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RWCDemo where

-----------------------------------------------------------------------------------

data Blah = Blah { myThing :: Int } deriving (Show, Eq)

wew :: Blah -> IO ()
wew Blah{..} = print myThing

-- |
--
-- e.g.
-- Prelude> wew $ Blah 3
-- 3
--
-- https://ocharles.org.uk/posts/2014-12-04-record-wildcards.html
-- https://kseo.github.io/posts/2014-02-10-record-wildcards.html
-- https://riptutorial.com/haskell/example/13072/recordwildcards

-----------------------------------------------------------------------------------




