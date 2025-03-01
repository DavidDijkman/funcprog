intersection :: (Eq a) => [a] -> [a] -> [a]

intersection xs [] = []
intersection xs (y:ys)
    | elem y xs = y : intersection xs ys


intersection1 :: (Eq a) => [a] -> [a] -> [a]

intersection1 xs ys = [y | y <- ys, y `elem` xs]

older :: Int -> [(String, Int)] -> [String]

older x ps = [name | (name, y) <- ps, y >= x]

sumseven :: [Int] -> [(Int, Int)]

sumseven xs = [(x, y) | x <- xs, y <- xs, even (x+y)]

ahead :: a -> [[a]] -> [[a]]

ahead x = map (x:)

dig2int :: [Int] -> Int
--(1:(0:(2:(1:[]))))
--(1:(0:(2:(1( )0))))


dig2int = foldr (\b a -> 10*a + b) 0

reveven :: [Int] -> [Int]

reveven = (foldr (\x a -> a ++ [x]) []).(filter even)


pells = 0:1: zipWith (+) (map (*2) (tail pells)) pells
{-
mapTree f (mapTree g t) = mapTree(f.g) t

base case t = Empty

mapTree f (Empty) = Empty = Empty

mapTree f (mapTree g t) = mapTree f (mapTree g (Node x t1 t2))
= mapTree f (Node (g x) (mapTree g t1) (mapTree g t2))
= Node ((f.g) x) (mapTree (f.g) t1) (mapTree (f.g) t2)
= mapTree (f.g) t1 t2

-}