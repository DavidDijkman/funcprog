

rationalToDecimalString :: Int -> Int -> String

rationalToDecimalString n d 
-- n: nominator, d: denominator, n > d, n < 10*d
    | n `mod` d == 0 = show (n `div` d)
    | otherwise = show (n `div` d) ++ "." ++ h (10*((n `mod` d) `div` g)) (d `div` g) 0 
        where 
            g = gcd n d
            h n d r
                | g == 1 && n == r || n == 0 = ""
                | g > 1 = show (n `div` d) ++ h' (d `div` g) 0
                | g == 1 && r == 0 = "(" ++ show (n `div` d) ++ h' d n ++ ")*"
                | otherwise = show (n `div` d) ++ h' d r
                    where 
                        g = gcd n d
                        h' = h (10*((n `mod` d) `div` g)) -- the first argument n is always the same
            
