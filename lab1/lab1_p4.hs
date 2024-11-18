
intervalSums :: Int -> Int

intervalSums n = g n (n `div` 2) (n `div` 2 + 1) (2* (n `div` 2) + 1) 0
    {-
    n: input
    a: lower bound of interval
    b: upper bound of interval
    t: interval sum
    c: amount of interval sums equal to n
    -}
    where 
        g n a b t c 
            | t < n && a <= 1 = c
            | t == n = g n (a-2) (b-1) (t-b+2*a-3) (c+1)
            | t > n = g n (a-1) (b-1) (t-b+a-1) c
            | t < n = g n (a-1) b (t+a-1) c




{-
intervalSums 21
g 21 10 11 21 0
g 21 10 10 11 1
g 21 9 10 19 1
g 21 8 10 27 1
g 21 7 9 24 1
g 21 6 8 21 1
g 21 6 7 13 2
g 21 5 7 18 2
g 21 4 7 22 2
g 21 3 6 18 2
g 21 2 6 20 2
g 21 1 6 21 2
g 21 1 5 15 3
3


-}