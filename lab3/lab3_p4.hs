
import Data.Char

data Token = TokVar String
           | TokNot        | TokAnd     | TokOr 
           | TokImplies    | TokFollows | TokEquiv
           | TokUnequal    | TokLpar    | TokRpar
           | TokEOF
  deriving (Show, Eq)

type Tokens = [Token]

lexer :: String -> Tokens
lexer []                       = [TokEOF]
-- implement the rest of lexer yourself.
lexer (c:s)
    | c == ' ' = lexer s
    | c == '~' = TokNot : lexer s 
    | c == '(' = TokLpar : lexer s
    | c == ')' = TokRpar : lexer s
    | otherwise = interpret [] (c:s)

interpret :: String -> String -> Tokens
interpret [] ('-':'>':s) = TokImplies : lexer s 
interpret [] ('<':'-':s) = TokFollows : lexer s 
interpret [] ('o':'r':' ':s) = TokOr : lexer s 
interpret [] ('a':'n':'d':' ':s) = TokAnd : lexer s
interpret [] ('=':'=':s) = TokEquiv : lexer s 
interpret [] ('/':'=':s) = TokUnequal : lexer s
interpret v [] = TokVar v : lexer []
interpret v (c:s)
    | isAlpha c || isNumber c = interpret (v ++ [c]) s
    | otherwise = TokVar v : lexer (c:s)