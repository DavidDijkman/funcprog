
{-
Let it be known that this code looked a lot cleaner before I had to adapt it. 
my original code wrote "p or q or r" as (p OR (q OR r))
while it had to be ((p OR q) OR r)
this made me have to add the countPriority function so I could find the last operator of equivalent priority
-}
import Data.Char
import Control.Concurrent.STM (check)

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

makeProperExpr :: [Token] -> [Token] -> Int -> Int -> Int -> PropExpr
{-
makeProperExpr lhs rhs par prio

This function scans over the list of tokens and subdivides the expression from lowest to highest priority operator.
lhs = the left hand side of the expression, which we have already checked
rhs = the right hand side, unchecked
par = how many parentheses 'deep' we are
prio = a measure of the priority operators we are looking for in this iteration
cnt = counts how many operators of the same priority we have encountered
fcnt = tracks where the final operator of the priority is
-}

--base case
makeProperExpr [] [TokVar s] 0 0 0 = Variable s
makeProperExpr [] (TokVar s:rhs) _ 5 0 = Variable s

makeProperExpr [] rhs _ prio 0 = makeProperExpr [] rhs 0 (prio+1) (countPriority rhs (prio+1) 0 0 )
--read over sections within parentheses
makeProperExpr lhs (TokLpar : rhs) pars prio cnt = makeProperExpr (lhs ++ [TokLpar]) rhs (pars+1) prio cnt
makeProperExpr lhs (TokRpar : rhs) pars prio cnt = makeProperExpr (lhs ++ [TokRpar]) rhs (pars-1) prio cnt
makeProperExpr lhs (TokEOF : rhs) pars _ cnt = makeProperExpr [] lhs 0 0 (countPriority lhs 0 0 0)


makeProperExpr lhs (tok : rhs) 0 priority 1
    | tok == TokEquiv && priority == 0 = checkOuterParentheses [] lhs 0 :==: checkOuterParentheses [] rhs 0
    | tok == TokUnequal && priority == 0 = Not (checkOuterParentheses [] lhs 0 :==: checkOuterParentheses [] rhs 0)
    | tok == TokImplies && priority == 1 = checkOuterParentheses [] lhs 0 :->: checkOuterParentheses [] rhs 0
    | tok == TokFollows && priority == 1 = checkOuterParentheses [] rhs 0 :->: checkOuterParentheses [] lhs 0
    | tok == TokOr && priority == 2 = checkOuterParentheses [] lhs 0 :|: checkOuterParentheses [] rhs 0
    | tok == TokAnd && priority == 3 = checkOuterParentheses [] lhs 0 :&: checkOuterParentheses [] rhs 0
    | tok == TokNot && priority == 4 = Not (checkOuterParentheses [] rhs 0)
    | otherwise = makeProperExpr (lhs ++ [tok]) rhs 0 priority 1 --continue reading

makeProperExpr lhs (tok : rhs) 0 priority cnt
    | tok == TokEquiv && priority == 0 = makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1)
    | tok == TokUnequal && priority == 0 = makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1)
    | tok == TokImplies && priority == 1 = makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1)
    | tok == TokFollows && priority == 1 = makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1)
    | tok == TokOr && priority == 2 = makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1)
    | tok == TokAnd && priority == 3 = makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1)
    | tok == TokNot && priority == 4 = Not (makeProperExpr (lhs ++ [tok]) rhs 0 priority (cnt-1))
    | otherwise = makeProperExpr (lhs ++ [tok]) rhs 0 priority cnt --continue reading

makeProperExpr lhs (tok : rhs) pars priority cnt --the part in parentheses, which is skipped
    = makeProperExpr (lhs ++ [tok]) rhs pars priority cnt

countPriority :: [Token] -> Int -> Int -> Int -> Int
countPriority [] prio par n = n
countPriority (TokLpar:rhs) prio par n = countPriority rhs prio (par+1) n
countPriority (TokRpar:rhs) prio par n = countPriority rhs prio (par-1) n
countPriority (tok:rhs) prio 0 n
    | tok == TokEquiv && prio == 0 = countPriority rhs prio 0 (n+1)
    | tok == TokUnequal && prio == 0 = countPriority rhs prio 0 (n+1)
    | tok == TokImplies && prio == 1 = countPriority rhs prio 0 (n+1)
    | tok == TokFollows && prio == 1 = countPriority rhs prio 0 (n+1)
    | tok == TokOr && prio == 2 = countPriority rhs prio 0 (n+1)
    | tok == TokAnd && prio == 3 = countPriority rhs prio 0 (n+1)
    | tok == TokNot && prio == 4 = countPriority rhs prio 0 (n+1)
    | otherwise = countPriority rhs prio 0 n  --continue reading

countPriority (tok:rhs) prio par n = countPriority rhs prio par n --read between parentheses

checkOuterParentheses :: [Token] -> [Token] -> Int -> PropExpr

checkOuterParentheses [] (tok:rhs) 0
    | tok == TokLpar = checkOuterParentheses [tok] rhs 1
    | otherwise = makeProperExpr [] (tok:rhs) 0 0 (countPriority (tok:rhs) 0 0 0)
checkOuterParentheses (open:lhs) [TokRpar] 1 = makeProperExpr [] lhs 0 0 (countPriority lhs 0 0 0)
checkOuterParentheses (open:lhs) [TokRpar, TokEOF] 1 = makeProperExpr [] lhs 0 0 (countPriority lhs 0 0 0)
checkOuterParentheses lhs rhs 0 = makeProperExpr [] (lhs ++ rhs) 0 0 (countPriority (lhs ++ rhs) 0 0 0)
checkOuterParentheses lhs (tok:rhs) par
    | tok == TokLpar = checkOuterParentheses (lhs ++ [tok]) rhs (par+1)
    | tok == TokRpar = checkOuterParentheses (lhs ++ [tok]) rhs (par-1)
    | otherwise = checkOuterParentheses (lhs ++ [tok]) rhs par

parser :: String -> PropExpr

parser s = checkOuterParentheses [] (init (lexer s)) 0
 

-- implement this function yourself
