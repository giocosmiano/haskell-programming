module MadLibsExercise where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

heSaid = "! he said "
heJumped = " as he jumped into his car "
heDrove = " and drove off with his "
hisWife = " wife."

{-
madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."
-}

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> heSaid <> adv <> heJumped <> noun <> heDrove <> adj <> hisWife

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat ([e, heSaid, adv, heJumped, noun, heDrove, adj, hisWife] :: [String])


-- e.g.
-- madlibbin'       "Ouch" "stupidly" "car" "brave"
-- madlibbinBetter' "Ouch" "stupidly" "car" "brave"

{-

https://en.wikipedia.org/wiki/Mad_Libs

"Ouch       ! he said stupidly as he jumped into his convertible car  and drove off with his brave     wife."

"___________! he said ______   as he jumped into his convertible ____ and drove off with his _________ wife."
 exclamation          adverb                                     noun                        adjective

-}
