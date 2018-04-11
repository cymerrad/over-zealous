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
    show t = show (coordinification t (Map.empty) 0 0)

type Coord a = Map.Map (Int, Int) a
coordinification :: Tree a -> Coord a -> Int -> Int -> Coord a
coordinification t acc d off =
    case t of
        Empty -> acc
        Node v left right ->
            let
                mLeft = coordinification left acc (d+1) (2*off)
            in
                Map.insert (d,off) v (coordinification right mLeft (d+1) (2*off+1))

padLeftToSize :: String -> Char -> Int -> String
padLeftToSize str c n | (length str - n) >= 0 = str | otherwise = padLeftToSize (c : str) c n

drawFromCoords :: Show a => Coord a -> [[String]]
drawFromCoords coords =
    let
        depth = fst . fst . Map.findMax $ coords
        rows = floor(2 ** depth)
        maxNodeSize = Map.foldl (\comp val -> (max (length (show val) ) comp)) 0 coords
    in
        [ 
            [ padLeftToSize (
                case (Map.lookup (d,r) coords) of
                    Nothing -> ""
                    Just v -> show v
                    ) ' ' maxNodeSize
            | d <- [1 .. depth]
            ]
        | r <- [1 .. rows]
        ]

tT1 = listToTree [ Just k | k <- [1 .. 20]]
tCoords = coordinification tT1 Map.empty 0 0