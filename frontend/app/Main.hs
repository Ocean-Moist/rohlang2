{-# LANGUAGE BlockArguments #-}
import Text.Read (readMaybe)
import Data.Char
data Combinators = S | K | I deriving (Show)

-- + - * / 6 min(int,int) max(int,int) & | ! % = !=
data Ops = Add | Sub | Mul | Div | Pow | Min | Max | And | Or | Not | Mod | Equals | NotEquals
  deriving (Show)

data Tokens where
  Op :: Ops -> Tokens
  Combinator :: Combinators -> Tokens
  OpenParen :: Tokens
  CloseParen :: Tokens
  deriving (Show)



data AST where
  Oper :: Ops -> AST -> AST -> AST
  Combi :: Combinators -> AST -> AST -> AST
  Input :: AST
  deriving (Show)

lexer :: String -> [Tokens]
lexer "" = []
lexer (c:cs)
  | c == ' ' = lexer cs -- Ignore whitespace
  | c == '(' = OpenParen : lexer cs
  | c == ')' = CloseParen : lexer cs
  | c `elem` "+-*/^&|!%=" = Op (parseOp [c]) : lexer cs
  | c == 'm' && cs `startsWith` "ax" = Op Max : lexer (drop 2 cs)
  | c == 'm' && cs `startsWith` "in" = Op Min : lexer (drop 2 cs)
  | c `elem` "<>" = case cs of
                      ('=':rest) -> Op (parseOp (c:"=")) : lexer rest
                      _ -> error "Invalid operator"
  | c == 'm' && cs `startsWith` "in" = Op Mod : lexer (drop 2 cs)
  | c == 's' = Combinator S : lexer cs
  | c == 'k' = Combinator K : lexer cs
  | c == 'i' = Combinator I : lexer cs
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
parseOp "min" = Min
parseOp "max" = Max

parseOp op = error $ "Invalid operator: " ++ op

prettyPrint :: AST -> String
prettyPrint ast = prettyPrint' ast 0

-- Helper function for pretty printing with indentation
prettyPrint' :: AST -> Int -> String
prettyPrint' Input indent = replicate indent ' ' ++ "Input\n"
prettyPrint' (Oper op left right) indent =
  replicate indent ' ' ++ show op ++ "\n" ++
  prettyPrint' left (indent + 2) ++
  prettyPrint' right (indent + 2)
prettyPrint' (Combi comb left right) indent =
  replicate indent ' ' ++ show comb ++ "\n" ++
  prettyPrint' left (indent + 2) ++
  prettyPrint' right (indent + 2)


main :: IO ()
main = do
  putStrLn "Enter an expression:"
  input <- getLine
  let tokens = lexer input
  print tokens
  -- let ast = parser tokens
  -- putStr (prettyPrint ast)

