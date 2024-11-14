import Text.XHtml (base)
import Distribution.Simple.Utils (xargs)
nAnd :: Bool -> Bool -> Bool

nAnd a b = not(a && b)


nAnd2 :: Bool -> Bool -> Bool

nAnd2 True True = False
nAnd2 x y = True


-- 3-8

mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not((m==n) && (n==p))


-- 3-14

min2 :: Int -> Int -> Int

min2 a b
    | a <= b = a
    | otherwise = b


minThree :: Int -> Int -> Int -> Int

minThree a b c = min2 a (min2 b c)

-- 3-17

charToNum :: Char -> Int

charToNum c
    | c < '0' = 0
    | c > '9' = 0
    | otherwise = fromEnum c - fromEnum '0'

numberNDroots :: Float -> Float -> Float -> Integer

numberNDroots a b c
    | discr < 0 = 0
    | discr == 0 = 1
    | otherwise = 2
      where discr = b*b - 4*a*c

numberRoots :: Float -> Float -> Float -> Integer

numberRoots a b c
    | a /= 0 = numberNDroots a b c
    | b /= 0 = 1
    | c == 0 = 3
    | otherwise = 0

-- 3.24

smallerRoot :: Float -> Float -> Float -> Float

smallerRoot a b c
    | nr == 0 = 0
    | nr == 3 = 0
    | a == 0 = -(c/b)
    | otherwise = (-b - sqrt (b*b - 4*a*c))/(2*a)
      where nr = numberRoots a b c

-- 4.17

rangeProduct :: Integer -> Integer -> Integer

rangeProduct m n 
    | n < m = 0
    | m == n = 1
    | otherwise = n*rangeProduct  m (n - 1) 

-- 4.18

fac :: Integer -> Integer

fac 0 = 1
fac n = rangeProduct 1 n

-- 4.32

pow2 :: Integer -> Integer


pow2 m
    | m == 0 = 1
    | mod m 2 == 0 = (pow2 (div m 2))^2
    | otherwise = 2*pow2 (m-1)

-- 5.1

maxOccurs :: Integer -> Integer -> (Integer, Integer)

maxOccurs a b
    | a == b = (a, 2)
    | a > b = (a, 1)
    | otherwise = (b, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)

maxThreeOccurs a b c
    | c < m = (m, cnt)
    | c == m = (m, cnt + 1)
    | otherwise = (c, 1)
      where (m, cnt) = maxOccurs a b

-- 5.18
doubleAll :: [Integer] -> [Integer]

doubleAll xs = [2*x | x <- xs]


-- 5.21

matches :: Integer -> [Integer] -> [Integer]

matches n xs = [n | x <- xs, x == n]

isElementOf :: Integer -> [Integer] -> Bool

isElementOf n xs = not(null (matches n xs))
