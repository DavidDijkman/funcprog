import Data.String

isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt x = h x 1
    where
        h x n
            | n^2 > x = n-1
            |otherwise = h x (n+1)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime k
    | even k = False
    | otherwise = null [x | x <- [3, 5..(isqrt k)], k `mod` x == 0]


{-
factors :: Integer -> [Integer]
factors n = [d | d <- [1 .. n `div` 2], n `mod` d == 0] ++ [n]

isPrime :: Integer -> Bool
isPrime n = [1,n] == factors n
-}

pal :: Integer -> Integer -> Integer

pal a b = h (a * 10 + b) a
    where
        h m 0 = m
        h m n = h (10*m + n `mod` 10) (n`div`10)

nthPalPrime :: Int -> Integer

nthPalPrime 1 = 2
nthPalPrime 2 = 3
nthPalPrime 3 = 5
nthPalPrime 4 = 7
nthPalPrime 5 = 11

nthPalPrime n = h [11, 7, 5, 3, 2] n 5 1 0
    where
        h (c:arr) n k a b
            | k >= n = c
            | b >= 10 = h (c:arr) n k (a+1) 0
            | isPrime p = h (p:c:arr) n (k+1) a (b+1)
            | otherwise = h (c:arr) n k a (b+1)
                where p = pal a b

