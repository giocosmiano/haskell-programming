module MonoidStateExercises where

import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

  mappend (Mem f) (Mem g) = Mem $ \ s ->
   let (a, s') = g s
       (a', s'') = f s'
   in  (a <> a', s'')

--  OR
--  mappend f g = Mem $ \ s ->
--   let (a, s') = runMem g s
--       (a', s'') = runMem f s'
--   in  (a <> a', s'')

f' = Mem $ \s -> ("hi", s + 1)

-- e.g.
-- in Prelude > main
-- ("hi",1)
-- ("hi",1)
-- ("",0)
-- True
-- True
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
