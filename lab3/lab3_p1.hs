{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

type Clause = ([Int], [Int])
type Clauses = [Clause]


    

resolutionProof :: Clauses -> Bool

resolutionProof clauses = foldr resolve ([], []) clauses == ([], [])

resolve :: Clause -> Clause -> Clause  --resolves two clauses

resolve (as, bs) (ps, qs) = (removeComp xs ys, removeComp ys xs)
    where 
        xs = (merge as ps)
        ys = (merge bs qs)

removeComp :: [Int] -> [Int] -> [Int]

removeComp [] [] = []
removeComp as [] = as
removeComp [] bs = []
removeComp (a:as) (b:bs)
    | a == b = removeComp as bs 
    | a < b = a : removeComp as (b:bs)
    | a > b = removeComp (a:as) bs

merge :: [Int] -> [Int] -> [Int] --merges two sorted lists, removing duplicates

merge [] [] = []
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs) 
    | a == b = a : merge as bs
    | a < b = a : merge as (b:bs)
    | a > b = b : merge (a:as) bs


