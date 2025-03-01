
selfrle :: [Int]

-- a: what it is trying to put down
-- b: how many it still has to put down
-- x: the next item in the list
selfrle = 1:2: f 2 1 []
    where

        f a 0 (x:xs) = x : f (a`mod`2+1) x xs
        f a b l = f a (b-1) (l++[a])
