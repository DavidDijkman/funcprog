import Data.Char
import Data.String

rpn2prefix :: String -> String

rpn2prefix "" = ""

rpn2prefix (c:str)
    | c == ' ' = c : rpn2prefix str
    | isNumber c =   rpn2prefix str 
    | not (isNumber c) = "((" ++  (c:")") ++ rpn2prefix str ++ ")"

