data Combinators = S | K | I


data Values where
  Empty :: Values
  Int :: Int -> Values
  String :: String -> Values
  Values :: (Values, Values) -> Values

data Ops where 
  Cat,
  Take  
    




main :: IO ()
main = putStrLn "Hello, Haskell!"
