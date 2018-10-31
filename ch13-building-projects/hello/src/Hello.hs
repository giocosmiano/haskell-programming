module Hello
  ( sayHello ) -- where we export the functions listed
  where

sayHello :: IO ()
sayHello = do
  putStrLn "hello world"
