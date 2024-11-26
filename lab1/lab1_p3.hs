

powMod :: Int -> Int -> Int -> Int

powMod n e d
    | e == 0 = 1
    | e == 1 = mod n d
    | e == 2*k = mod (powMod (mod (n^2) d) k d) d
    | otherwise = mod (n * powMod (mod n d) (e-1) d) d
        where 
            k = div e 2

composite :: Int -> Int



composite n
    | n == 1 = 0
    | even n = 1
    | otherwise = p n 3
        where
            p n a 
                | a*a > n = 0
                | n `mod` a == 0 = 1
                | otherwise = p n (a+2)


poulet :: Int -> Int

poulet n = p n 5 0
    where 
        p n i a
            | n == 0 = a
            | (powMod 2 (i-1) i == 1) && (composite i == 1) = p (n-1) (i+2) i
            | otherwise = p n (i+2) a

