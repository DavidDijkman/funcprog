import Data.Char

readS :: ([String], [String]) -> String -> String -> Bool -> ([String], [String])
--the boolean denotes whether we are in the premises or the goals, we check this by counting '['s

readS split _ [] _ = split
readS (ps, gs) phr ('[':s) b
    | b = readS (ps, gs) [] s False
    | not b = readS (ps, gs) [] s True

readS  (ps, gs) phr (c:s) b
    | (c == ';' || c == ']') && phr == "" = readS (ps, gs) [] s b
    | (c == ';' || c == ']') && not b = readS (ps ++ [removeSpaces phr False], gs) [] s b
    | (c == ';' || c == ']') &&     b = readS (ps, gs ++ [removeSpaces phr False]) [] s b
    | otherwise = readS (ps, gs) (phr ++ [c]) s b

removeSpaces :: String -> Bool -> String 
--removes unnecessary spaces from the sentences
removeSpaces [] _ = []
removeSpaces " " _ = []
removeSpaces (' ':c:s) b
    | (isAlpha c || c == '~' || c == '(') && b = ' ' : removeSpaces (c:s) True 
    | (isAlpha c || c == '~' || c == '(') && not b = removeSpaces (c:s) True 
    | otherwise = removeSpaces (c:s) False 
removeSpaces (c:s) b
    | c == '\n' = removeSpaces (' ':s) b --can't be bothered, just replace newlines with spaces
    | isAlpha c || c == ')' = c : removeSpaces s True
    | otherwise = c : removeSpaces s False

splitInput :: String -> ([String], [String])
splitInput s = readS ([], []) [] s True