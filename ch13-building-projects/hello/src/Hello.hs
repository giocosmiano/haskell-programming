module Hello
  ( sayHello ) -- where we export the functions listed
  where

sayHello :: String -> IO ()
sayHello name = do
  putStrLn $ "Hi " ++ name ++ "!"
