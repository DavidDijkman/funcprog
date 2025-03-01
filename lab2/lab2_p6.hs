
numPals :: [Integer]

numPals = h 9 0 1
    where 
        h :: Integer -> Integer -> Integer -> [Integer]
        h a b n 
            | odd n = (a+b):h a (b+a) 0
            | otherwise = (a+b):h (10*a) (b+a) 1


