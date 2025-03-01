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
abortParser :: String -> (String,[String])
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
-}


lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t "    = lexer cs -- skip whitespace
  | elem c "+-*/%()"  = [c]:lexer cs
  | isDigit c         = (c:takeWhile isDigit cs):lexer (dropWhile isDigit cs)
  | otherwise         = abortLexer $ "Lexical error: invalid character '" ++ [c] ++ "' in input"

-- rule: S  -> E
parseS :: String -> [String] -> (String,[String])
parseS = parseE

-- rule: E  -> T E'
parseE :: String -> [String] -> (String,[String])
parseE accepted tokens = parseE' acc rest
  where (acc, rest) = parseT accepted tokens

-- rules: E' -> + T E'
--        E' -> - T E'
--        E' -> <empty string>
parseE' :: String -> [String] -> (String,[String])
parseE' accepted ("+":tokens) =  
  let (acc,rest) = parseT (accepted++"+") tokens 
    in parseE' acc rest
parseE' accepted ("-":tokens) =  
  let (acc,rest) = parseT (accepted++"-") tokens 
    in parseE' acc rest
parseE' accepted tokens = (accepted, tokens)

-- rule: T  -> F T'
parseT :: String -> [String] -> (String, [String])
parseT accepted tokens = parseT' acc rest
  where (acc, rest) = parseF accepted tokens

-- rules: T' -> * F T'
--        T' -> / F T'
--        T' -> % F T'
--        T' -> <empty string>
parseT' :: String -> [String] -> (String,[String])
parseT' accepted ("*":tokens) =  
  let (acc,rest) = parseF (accepted++"*") tokens 
    in parseT' acc rest
parseT' accepted ("/":tokens) =  
  let (acc,rest) = parseF (accepted++"/") tokens 
    in parseT' acc rest
parseT' accepted ("%":tokens) =  
  let (acc,rest) = parseF (accepted++"%") tokens 
    in parseT' acc rest
parseT' accepted tokens =  (accepted, tokens)

expect :: String -> String-> [String] -> (String, [String])
expect _ _ [] = abortParser "Parse error...truncated input....abort"
expect exp accepted (tok:tokens)
 | exp == tok = (accepted++exp,tokens)
 | otherwise  = abortParser $ "Parse error...expected '" ++ exp ++ "' but encountered '" ++ tok ++ "' instead....abort"
 
-- rules: F -> (E)
--        F -> <digits>
parseF :: String -> [String] -> (String, [String])
parseF accepted []      =  abortParser "Parse error...truncated input....abort"
parseF accepted ("(":tokens) =
  let (acc,rest) = parseE (accepted++"(") tokens 
    in expect ")" acc rest
parseF accepted (tok:tokens)
  | isDigit (head tok)  =  (accepted++tok, tokens)
  | otherwise           =  abortParser $ "Syntax Error: unexpected '" ++ tok ++ "'"

parser :: String -> (String,[String])
parser str = parseS "" (lexer str)
