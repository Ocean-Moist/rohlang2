import System.Environment
import System.IO
import Text.Read
import GHC.IO

data Ops = Add | Sub | Div | Mul | Pow | Min | Max | And | Or | Not | Mod | Eq deriving (Show, Eq)

data Tokens = Op Ops | S Tokens Tokens Tokens | K Tokens Tokens deriving (Show, Eq)

data AST = Operator Ops AST AST | Input deriving (Show, Eq)

lexer :: String -> [Tokens]
lexer input = lexer' input []
  where
    lexer' :: String -> [Tokens] -> [Tokens]
    lexer' [] tokens = tokens
    lexer' (c:cs) tokens =
      case c of
        ' ' -> lexer' cs tokens
        '\n' -> lexer' cs tokens
        '+' -> lexer' cs (tokens ++ [Op Add])
        '-' -> lexer' cs (tokens ++ [Op Sub])
        '*' -> lexer' cs (tokens ++ [Op Mul])
        '/' -> lexer' cs (tokens ++ [Op Div])
        '^' -> lexer' cs (tokens ++ [Op Pow])
        '&' -> lexer' cs (tokens ++ [Op And])
        '|' -> lexer' cs (tokens ++ [Op Or])
        '!' -> lexer' cs (tokens ++ [Op Not])
        '%' -> lexer' cs (tokens ++ [Op Mod])
        '=' -> lexer' cs (tokens ++ [Op Eq])
        'm' ->
          case cs of
            'i' : cs' ->
              case cs' of
                'n' : cs'' -> lexer' cs'' (tokens ++ [Op Min])
                _ -> error "Invalid character"
            'a' : cs' ->
              case cs' of
                'x' : cs'' -> lexer' cs'' (tokens ++ [Op Max])
                _ -> error "Invalid character"
            _ -> error "Invalid character"
        's' ->
          let (s1, cs') = lexerSingle cs
              (s2, cs'') = lexerSingle cs'
              (s3, cs''') = lexerSingle cs''
           in lexer' cs''' (tokens ++ [S s1 s2 s3])
        'k' ->
          let (k1, cs') = lexerSingle cs
              (k2, cs'') = lexerSingle cs'
           in lexer' cs'' (tokens ++ [K k1 k2])
        _ -> error "Invalid character"

lexerSingle :: String -> (Tokens, String)
lexerSingle (c:cs) =
  case c of
    ' ' -> lexerSingle cs
    '\n' -> lexerSingle cs
    '+' -> (Op Add, cs)
    '-' -> (Op Sub, cs)
    '*' -> (Op Mul, cs)
    '/' -> (Op Div, cs)
    '^' -> (Op Pow, cs)
    '&' -> (Op And, cs)
    '|' -> (Op Or, cs)
    '!' -> (Op Not, cs)
    '%' -> (Op Mod, cs)
    '=' -> (Op Eq, cs)
    'm' ->
      case cs of
        'i' : cs' ->
          case cs' of
            'n' : cs'' -> (Op Min, cs'')
            _ -> error "Invalid character"
        'a' : cs' ->
          case cs' of
            'x' : cs'' -> (Op Max, cs'')
            _ -> error "Invalid character"
        _ -> error "Invalid character"
    's' ->
      let (s1, cs') = lexerSingle cs
          (s2, cs'') = lexerSingle cs'
          (s3, cs''') = lexerSingle cs''
       in (S s1 s2 s3, cs''')
    'k' ->
      let (k1, cs') = lexerSingle cs
          (k2, cs'') = lexerSingle cs'
       in (K k1 k2, cs'')
    _ -> error "Invalid character"
lexerSingle _ = error "Invalid character"

lexOp :: Char -> Ops
lexOp c =
  case c of
    '+' -> Add
    '-' -> Sub
    '*' -> Mul
    '/' -> Div
    '^' -> Pow
    '&' -> And
    '|' -> Or
    '!' -> Not
    '%' -> Mod
    '=' -> Eq
    _ -> error "Invalid operator"

unboxTokens :: Tokens -> [Tokens]
unboxTokens (Op op) = [Op op]
unboxTokens (S t1 t2 t3) = unboxTokens t1 ++ unboxTokens t2 ++ unboxTokens t3
unboxTokens (K t1 t2) = unboxTokens t1 ++ unboxTokens t2

parser :: [Tokens] -> AST
parser tokens = parser' tokens Input
  where
    parser' :: [Tokens] -> AST -> AST
    parser' [] ast = ast
    parser' (t:ts) ast =
      case t of
        S x y z ->
          let xz = replaceInputNode (parser' (unboxTokens x) ast) (parser' (unboxTokens z) ast)
              yz = replaceInputNode (parser' (unboxTokens y) ast) (parser' (unboxTokens z) ast)
           in parser' ts (replaceInputNode xz yz)
        K x _ -> parser' ts (replaceInputNode ast (parser' (unboxTokens x) ast))
        Op op -> parser' ts (replaceInputNode ast (generateOpNodeStructure op))

replaceInputNode :: AST -> AST -> AST
replaceInputNode ast input = replaceInputNode' [ast]
  where
    replaceInputNode' :: [AST] -> AST
    replaceInputNode' [] = ast
    replaceInputNode' (n:ns) =
      case n of
        Operator op left right ->
          if left == Input
            then Operator op input right
            else if right == Input
              then Operator op left input
              else replaceInputNode' (ns ++ [left, right])
        Input -> input

generateOpNodeStructure :: Ops -> AST
generateOpNodeStructure op =
  case op of
    Add -> Operator Add Input Input
    Sub -> Operator Sub Input Input
    Mul -> Operator Mul Input Input
    Div -> Operator Div Input Input
    Pow -> Operator Pow Input Input
    Min -> Operator Min Input Input
    Max -> Operator Max Input Input
    And -> Operator And Input Input
    Or -> Operator Or Input Input
    Not -> Operator Not Input Input
    Mod -> Operator Mod Input Input
    Eq -> Operator Eq Input Input

eval :: AST -> Int
eval (Operator op left right) =
  let leftValue = eval left
      rightValue = eval right
   in case op of
        Add -> leftValue + rightValue
        Sub -> leftValue - rightValue
        Mul -> leftValue * rightValue
        Div -> leftValue `div` rightValue
        Pow -> leftValue ^ rightValue
        Min -> min leftValue rightValue
        Max -> max leftValue rightValue
        And -> if leftValue /= 0 && rightValue /= 0 then 1 else 0
        Or -> if leftValue /= 0 || rightValue /= 0 then 1 else 0
        Not -> if leftValue == 0 then 1 else 0
        Mod -> leftValue `mod` rightValue
        Eq -> if leftValue == rightValue then 1 else 0
eval Input =
  -- Prompt the user for input until a valid integer is entered
  let readInput :: IO Int
      readInput = do
        putStrLn "Enter an integer:"
        input <- getLine
        case readMaybe input of
          Just value -> return value
          Nothing -> do
            putStrLn "Invalid input! Please enter a valid integer."
            readInput
   in unsafePerformIO readInput

main :: IO ()
main = do
  args <- getArgs 
  -- open .roh file and read it into a string
  input <- readFile (head args)
  let tokens = lexer input -- Tokenize input string
      ast = parser tokens -- Parse tokens into AST
  print (eval ast)
