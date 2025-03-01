{- These imports (System.*) are necessary for error reporting followed by an abort.
 - The way this is performed is a hack, and certainly not a clean functional style.
 - However, accept it and only use it for error reporting (it is better than using "error").
 -} 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import System.IO.Unsafe
import System.Exit

-- Data.Char is quite useful. It offers functions like "isAlpha", etc.
import Data.Char


-----------------------------------------------------------------------------------------------

{- the abortParser and abortLexer 'functions' are hacky. Just accept it.
 - they actually do not return. They print an error message and gracefully abort the program.
 -}
abortParser :: String -> (Integer,[String])
-- the type of this function is to keep the type system happy.
abortParser errMessage = unsafePerformIO (die errMessage)

abortLexer :: String -> [String]
-- the type of this function is to keep the type system happy.
abortLexer errMessage = unsafePerformIO (die errMessage)


-----------------------------------------------



{- parser for the grammar:
    S  -> E
    E  -> T E'
    E' -> + T E'
    E' -> - T E'
    E' -> <empty string>
    T  -> F T'
    T' -> * F T' | / F T' | % F T' | epsilon
    F  -> (E) | <digits>

parser "2+2"
parseS "" ["2", "+", "2"]
parseE "" ["2", "+", "2"]
parseT "" ["2", "+", "2"]

parseT' acc rest
  (acc, rest) = ParseF "" ["2", "+", "2"]

parseT' "2" ["+", "2"]

parseE' "2" ["+", "2"]

parseE' = parseT' acc rest 
  (acc1, rest1) = parse F "2+" ["2"]

parseE' = parseT' "2+2" []

("2+2", [])   
-}



lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t "    = lexer cs -- skip whitespace
  | elem c "+-*/%()"  = [c]:lexer cs
  | isDigit c         = (c:takeWhile isDigit cs):lexer (dropWhile isDigit cs)
  | otherwise         = abortLexer $ "Lexical error: invalid character '" ++ [c] ++ "' in input"

-- rule: S  -> E
parseS :: Integer -> [String] -> (Integer,[String])
parseS = parseE

-- rule: E  -> T E'
parseE :: Integer -> [String] -> (Integer,[String])
parseE accepted tokens = parseE' acc rest
  where (acc, rest) = parseT accepted tokens

-- rules: E' -> + T E'
--        E' -> - T E'
--        E' -> <empty string>
parseE' :: Integer -> [String] -> (Integer,[String])
parseE' accepted ("+":tokens) =  
  let (acc,rest) = parseT accepted tokens 
    in parseE' (accepted + acc) rest
parseE' accepted ("-":tokens) =  
  let (acc,rest) = parseT accepted tokens 
    in parseE' (accepted - acc) rest
parseE' accepted tokens =  (accepted, tokens)

-- rule: T  -> F T'
parseT :: Integer -> [String] -> (Integer, [String])
parseT accepted tokens = parseT' acc rest
  where (acc, rest) = parseF accepted tokens

-- rules: T' -> * F T'
--        T' -> / F T'
--        T' -> % F T'
--        T' -> <empty string>
parseT' :: Integer -> [String] -> (Integer,[String])
parseT' accepted ("*":tokens) =  
  let (acc,rest) = parseF accepted tokens 
    in parseT' (accepted * acc) rest
parseT' accepted ("/":tokens) =  
  let (acc,rest) = parseF accepted tokens 
    in parseT' (accepted `div` acc) rest
parseT' accepted ("%":tokens) =  
  let (acc,rest) = parseF accepted tokens 
    in parseT' (accepted `mod` acc) rest
parseT' accepted tokens =  (accepted, tokens)

expect :: String -> Integer -> [String] -> (Integer, [String])
expect _ _ [] = abortParser "Parse error...truncated input....abort"
expect exp accepted (tok:tokens)
 | exp == tok = (accepted,tokens)
 | otherwise  = abortParser $ "Parse error...expected '" ++ exp ++ "' but encountered '" ++ tok ++ "' instead....abort"
 
-- rules: F -> (E)
--        F -> <digits>
parseF :: Integer -> [String] -> (Integer, [String])
parseF accepted []      =  abortParser "Parse error...truncated input....abort"
parseF accepted ("(":tokens) =
  let (acc,rest) = parseE accepted tokens 
    in expect ")" acc rest
parseF accepted (tok:tokens)
  | isDigit (head tok)  =  (read tok, tokens)
  | otherwise           =  abortParser $ "Syntax Error: unexpected '" ++ tok ++ "'"

parser :: String -> (Integer,[String])
parser str = parseS 0 (lexer str)

parseEvalExpr :: String -> Integer
parseEvalExpr str = fst (parser str)