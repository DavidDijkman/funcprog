import Data.Char


findVars :: [String] -> String -> [String]

findVars vars [] = vars
findVars vars (c:s)
    | not (isAlpha c) = findVars vars s
    | otherwise = readVar vars [] (c:s)

readVar vars var (c:s)
    | isAlpha c || isNumber c = readVar vars (var ++ [c]) s
    | var `elem` vars = findVars vars (c:s)
    | lvar == "entails" || lvar == "or" || lvar == "and" = findVars vars (c:s)
    | otherwise = findVars (insertVar var vars) (c:s)
        where lvar = map toLower var

insertVar :: String -> [String] -> [String]
insertVar var [] = [var]
insertVar var (var1:vars)
    | isHigher (zip var var1) (length var > length var1) = var1 : insertVar var vars
    | otherwise = var : var1 : vars

isHigher :: [(Char, Char)] -> Bool -> Bool
isHigher [] l = l
isHigher (z:zipped) l
    | fst z == snd z = isHigher zipped l
    | fst z > snd z = True
    | fst z < snd z = False



variables :: String -> [(Int, String)]

variables s = zip [0..] (findVars [] s)



