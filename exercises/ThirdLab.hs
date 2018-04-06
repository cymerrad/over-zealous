module ThirdLab where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

safeGet :: [a] -> Int -> Maybe a
safeGet l i | length l <= i = Nothing | otherwise = Just (l!!i)

listToTree :: [a] -> Tree a
listToTree [] = Empty
listToTree [x] = Node x Empty Empty
listToTree (x:xs) = 


depth :: Tree a -> Int
depth t =
    case t of
        Empty -> 0
        Node _ t1 t2 -> max (depth t1) (depth t2) + 1

-- instance Show a => Show (Tree a) where
--     show t = case t of

 
--  instance Eq a => Eq (Tree a) where
--     t1 == t2 = ...