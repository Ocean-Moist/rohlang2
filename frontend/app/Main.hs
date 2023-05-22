import Text.Read (readMaybe)
import Data.Char
data Combinators = S | K | I

data Value where
  Empty :: Value
  Int :: Int -> Value
  Values :: (Value, Value) -> Value

-- + - * / 6 min(int,int) max(int,int) & | ! % = !=
data Ops = Add | Sub | Mul | Div | Pow | Min | Max | And | Or | Not | Mod | Equals | NotEquals

data Tokens where 
  Value :: Value -> Tokens
  Op :: Ops -> Tokens
  Combinator :: Combinators -> Tokens
  OpenParen :: Tokens
  CloseParen :: Tokens

lexer :: String -> [Tokens]
lexer "" = []
lexer (c:cs)
  | c == ' ' = lexer cs -- Ignore whitespace
  | c == '(' = OpenParen : lexer cs
  | c == ')' = CloseParen : lexer cs
  | c `elem` "+-*/^&|!%=" = Op (parseOp [c]) : lexer cs
  | c `elem` "<>" = case cs of
                      ('=':rest) -> Op (parseOp (c:"=")) : lexer rest
                      _ -> error "Invalid operator"
  | c == 'm' && cs `startsWith` "in" = Op Mod : lexer (drop 2 cs)
  | c == 'S' = Combinator S : lexer cs
  | c == 'K' = Combinator K : lexer cs
  | c == 'I' = Combinator I : lexer cs
  | isDigit c = case readMaybe (c : takeWhile isDigit cs) of
                  Just n -> Value (Int n) : lexer (dropWhile isDigit cs)
                  Nothing -> error "Invalid number"
  | otherwise = error "Invalid character"

-- Helper function to check if a string starts with a specific substring
startsWith :: String -> String -> Bool
startsWith str sub = take (length sub) str == sub

-- Helper function to parse an operator based on its string representation
parseOp :: String -> Ops
parseOp "+" = Add
parseOp "-" = Sub
parseOp "*" = Mul
parseOp "/" = Div
parseOp "^" = Pow
parseOp "&" = And
parseOp "|" = Or
parseOp "!" = Not
parseOp "%" = Mod
parseOp "=" = Equals
parseOp "!=" = NotEquals
parseOp op = error $ "Invalid operator: " ++ op

