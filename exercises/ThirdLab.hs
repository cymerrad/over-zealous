module ThirdLab where

import FirstLab ( flatten )

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

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
    

rightBranch list = []

listToTree :: [Maybe a] -> Tree a
listToTree [] = Empty
-- listToTree [x] = Node x Empty Empty
listToTree (x:xs) = 
    case x of
        Nothing -> Empty
        Just v -> Node v (listToTree (leftBranch xs)) (listToTree (rightBranch xs))

-- depth :: Tree a -> Int
-- depth t =
--     case t of
--         Empty -> 0
--         Node _ t1 t2 -> max (depth t1) (depth t2) + 1

-- instance Show a => Show (Tree a) where
--     show t = case t of

 
--  instance Eq a => Eq (Tree a) where
--     t1 == t2 = ...