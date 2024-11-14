powDigits :: Integer -> Integer -> Int -> Integer

powDigits n e d
    | e == 0 = 1
    | e == 1 = mod n p
    | e == 2*k = mod (powDigits (mod (n^2) p) k d) p
    | otherwise = mod (n * powDigits (mod n p) (e-1) d) p

    where 
     p = 10^d
     k = div e 2