fibs :: [Integer] -- taken from lecture slides
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)




cats :: [Integer]
cats = 1: [sum (zipWith (*) (take n cats) (reverse (take n cats))) | n <- [1..]]

fibcat :: [Integer]

fibcat = h cats fibs (-1)
    where 
        h (f:fs) (c:cs) m
            | f == m = h fs (c:cs) f
            | c == m = h (f:fs) cs c
            | f < c = f : h fs (c:cs) f
            | f > c = c : h (f:fs) cs c
            | f == c = c : h fs cs f

