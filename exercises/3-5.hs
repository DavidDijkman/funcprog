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