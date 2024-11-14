
fusc :: Integer -> Integer

fusc 0 = 0
fusc 1 = 1

fusc n = f n 1 0
    where
      f 0 a b = b
      f n a b
        | n == 2*k = f k (a + b) b
        | n == 2*k + 1 = f k a (b + a)
          where
            k = div n 2