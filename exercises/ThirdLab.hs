module ThirdLab where

import FirstLab ( flatten )
import qualified Data.Map as Map

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord) --, Show)

safeGet :: [a] -> Int -> Maybe a
safeGet l i | length l <= i = Nothing | otherwise = Just (l!!i)

lefties = flatten 
    [
        [ floor(2**k - 1 + m) 
        | m <- [0 .. (2**k)/2 - 1] 
        ] 
    | k <- [1 .. ] 
    ]

leftBranch :: [Maybe a] -> [Maybe a]
leftBranch list =
    map snd (filter (\p -> fst p `elem` take (length list) lefties) (zip [1 .. ] list))
    
rightBranch :: [Maybe a] -> [Maybe a]
rightBranch list = 
    map snd (filter (\p -> fst p `notElem` take (length list) lefties) (zip [1 .. ] list))

listToTree :: [Maybe a] -> Tree a
listToTree [] = Empty
-- listToTree [x] = Node x Empty Empty
listToTree (x:xs) = 
    case x of
        Nothing -> Empty
        Just v -> Node v (listToTree (leftBranch xs)) (listToTree (rightBranch xs))

depth :: Tree a -> Int
depth t =
    case t of
        Empty -> 0
        Node _ t1 t2 -> max (depth t1) (depth t2) + 1

instance Show a => Show (Tree a) where
    show t = show (stratification t (Map.empty) 0 0)

type Strat a = Map.Map Int [(a, Int)]
stratification :: Tree a -> Strat a -> Int -> Int -> Strat a
stratification t acc d off =
    case t of
        Empty -> acc
        Node v left right ->
            let
                mLeft = stratification left acc (d+1) (2*off)
            in
                Map.insertWith (flip (++)) d [(v,off)] (stratification right mLeft (d+1) (2*off+1))

-- drawFromStrat :: Show a => Strat a -> String
-- drawFromStrat strat =
--     let
--         rows = 2 ** (Map.findMax strat)
--     in
        

tT1 = listToTree [ Just k | k <- [1 .. 20]]