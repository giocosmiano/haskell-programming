module Listy where

newtype Listy a = Listy [a] deriving (Eq, Show)

-- testing why orphan instances are problematic

--ghc -I. --make ListyInstances.hs

--instance Monoid (Listy a) where
--  mempty = Listy []
--  mappend (Listy l) (Listy l') = Listy $ mappend l l'

{-
Listy.hs:5:10: error:
    Duplicate instance declarations:
      instance [safe] Monoid (Listy a) -- Defined at Listy.hs:5:10
      instance Monoid (Listy a) -- Defined at ListyInstances.hs:6:10
-}
