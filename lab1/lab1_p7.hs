maxT :: [(Integer, Integer)] -> (Integer, Integer)

maxT a = h a 0 0
    where
        h [] m n = (m, n)
        h (x:b) m n
            | snd x > n = h b (fst x) (snd x)
            | otherwise = h b m n

prod :: [Integer] -> Integer

prod a = h a 1
    where
        h [] m = m
        h (x:b) m = h b (m*x)

solveModularEq :: [(Integer,Integer)] -> Integer

solveModularEq a = h (prod [snd x | x <- a]) a (snd (maxT a))  (fst (maxT a))
    where
        h mx a n x
            | x >= mx = mx
            | and [x`mod`q == p |(p, q) <- a] = x
            | otherwise = h mx a n (x+n)
