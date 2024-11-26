import Data.Char


or2 :: [Bool] -> Bool

or2 (True:_) = True
or2 (_:xs) = or2 xs
or2 [] = False

--7.8

elemNum :: Integer -> [Integer] -> Integer

elemNum _ [] = 0
elemNum x (y:ys)
    | x == y = 1 + elemNum x ys
    | otherwise = elemNum x ys

elemNum2 :: Eq a => a -> [a] -> Int
elemNum2 x xs = length (filter (==x) xs)

--7.9

unique :: [Integer] -> [Integer]

unique [] = []
unique (x:xs)
    | exists = unique rest
    | otherwise = x:(unique rest)
     where
        (exists, rest) = other x xs
        other _ [] = (False, [])
        other x (y:ys)
         | x == y = (True, rest2)
         | otherwise = (exists2, rest2)
            where 
                (exists2, rest2) = other y ys

--7.16

iSort :: [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
 | x == y = y:ys
 | x > y = x:(y:ys)
 | otherwise = y:(ins x ys)

--7.25

sublist :: Eq a => [a] -> [a] -> Bool

sublist [] _ = True
sublist _ [] = False
sublist (x:xs) (y:ys)
    | x == y = sublist xs ys
    | otherwise = sublist (x:xs) (ys)

subsequence :: Eq a => [a] -> [a] -> Bool 

subsequence [] _ = True
subsequence _ [] = False
subsequence (x:xs) (y:ys) 
 | zip xs ys == zip xs xs = True
 | otherwise = subsequence (x:xs) (ys)

--7.33

isPalin :: String -> Bool

isPalin s = reverse s == s
    where
        filtered = [toLower x | x <- s, isAlpha x]

--7.34

subst :: String -> String -> String -> String

subst _ _ [] = []
subst needle replace (h:aystack)
    | zip needle (h:aystack) == zip needle needle = replace ++ (drop (length needle) (h:aystack))
    | otherwise = h:(subst needle replace aystack)

