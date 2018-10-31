module DogsRule
  ( dogs ) -- where we export the functions listed
  where

dogs :: IO ()
dogs = do
  putStrLn "Who's a good puppy?!"
  putStrLn "YOU ARE!!!!!"
