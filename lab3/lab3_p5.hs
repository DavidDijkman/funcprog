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

data PropExpr = Not PropExpr
              | PropExpr :->: PropExpr
              | PropExpr :&:  PropExpr
              | PropExpr :|:  PropExpr
              | PropExpr :==: PropExpr
              | Variable String

instance Show PropExpr where
  show (Not (Variable v)) = "~" ++ v
  show (Not e)   = "~(" ++ show e ++ ")"
  show (lhs :&: rhs)  = "(" ++ show lhs ++ " AND " ++ show rhs ++ ")"
  show (lhs :|: rhs)  = "(" ++ show lhs ++ " OR " ++ show rhs ++ ")"
  show (lhs :->: rhs) = "(" ++ show lhs ++ " -> " ++ show rhs ++ ")"
  show (lhs :==: rhs) = "(" ++ show lhs ++ " == " ++ show rhs ++ ")"
  show (Variable var) = var

makeProperExpr :: [Token] -> [Token] -> Int -> Int -> PropExpr
{-
makeProperExpr lhs rhs par prio

This function scans over the list of tokens and subdivides the expression from lowest to highest priority operator.
lhs = the left hand side of the expression, which we have already checked
rhs = the right hand side, unchecked
par = how many parentheses 'deep' we are
prio = a measure of the priority operators we are looking for in this iteration
-}

--base case
makeProperExpr [] [TokVar s] 0 0 = Variable s
makeProperExpr [] (TokLpar:rhs) 0 0 = checkOuterParentheses [] rhs 1
--read over sections within parentheses
makeProperExpr lhs (TokLpar : rhs) pars prio = makeProperExpr (lhs ++ [TokLpar]) rhs (pars+1) prio
makeProperExpr lhs (TokRpar : rhs) pars prio = makeProperExpr (lhs ++ [TokRpar]) rhs (pars-1) prio
makeProperExpr lhs (TokEOF : rhs) pars _ = makeProperExpr [] lhs 0 0
makeProperExpr lhs [] _ prio = makeProperExpr [] lhs 0 (prio+1)

makeProperExpr lhs (tok : rhs) 0 priority
    | tok == TokEquiv && priority == 0 = makeProperExpr [] lhs 0 0 :==: makeProperExpr [] rhs 0 0
    | tok == TokUnequal && priority == 0 = Not (makeProperExpr [] lhs 0 0 :==: makeProperExpr [] rhs 0 0)
    | tok == TokImplies && priority == 1 = makeProperExpr [] lhs 0 0 :->: makeProperExpr [] rhs 0 0
    | tok == TokFollows && priority == 1 = makeProperExpr [] rhs 0 0 :->: makeProperExpr [] lhs 0 0
    | tok == TokOr && priority == 2 = makeProperExpr [] lhs 0 0 :|: makeProperExpr [] rhs 0 0
    | tok == TokAnd && priority == 3 = makeProperExpr [] lhs 0 0 :&: makeProperExpr [] rhs 0 0
    | tok == TokNot && priority == 4 = Not (makeProperExpr [] rhs 0 0)
    | otherwise = makeProperExpr (lhs ++ [tok]) rhs 0 priority --continue reading

makeProperExpr lhs (tok : rhs) pars priority --the part in parentheses, which is skipped
    = makeProperExpr (lhs ++ [tok]) rhs pars priority

checkOuterParentheses :: [Token] -> [Token] -> Int -> PropExpr

checkOuterParentheses lhs [TokRpar] 1 = makeProperExpr [] lhs 0 0
checkOuterParentheses lhs rhs 0 = makeProperExpr [] (TokLpar:lhs) 0 0
checkOuterParentheses lhs (tok:rhs) par
    | tok == TokLpar = checkOuterParentheses (lhs ++ [tok]) rhs (par+1)
    | tok == TokRpar = checkOuterParentheses (lhs ++ [tok]) rhs (par-1)
    | otherwise = checkOuterParentheses (lhs ++ [tok]) rhs par

parser :: String -> PropExpr

parser s = makeProperExpr [] (lexer s) 0 0


-- implement this function yourself
