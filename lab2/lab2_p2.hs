smallestMultiple :: Integer -> Integer

--lcm is least common multiple
--foldr computes the least common multiple of all the digits 1..n
smallestMultiple n = foldr lcm 1 [1..n]