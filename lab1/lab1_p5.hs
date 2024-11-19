
collatzSteps :: Integer -> Integer

collatzSteps n = c n n 0
    where
        c n 1 s = s
        c n m s 
            | m == 2*k = c n k (s+1)
            | otherwise = c n (3*m + 1) (s+1)
                where k = m `div` 2


nthCollatzRecord :: Int -> Integer

nthCollatzRecord n = g n 1 0 0
    where
        {-
        n: input
        a: incrementer
        r: collatz record up to a
        m: counts amount of records
        -}
        --g :: Int -> Integer -> Integer -> Integer  -> Integer
        g n a r m 
            | n == 0 = 0
            | k > r && m+1 == n = a
            | k > r = g n (a+1) k (m+1) 
            | otherwise = g n (a+1) r m
                where k = collatzSteps a


