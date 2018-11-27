module ThirdLab where

-- import Prelude hiding(Either(..))
import           Exercises.FirstLab             ( flatten )
import qualified Data.Map                      as Map
import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Data.Maybe                     ( catMaybes )
import           Text.Read                      ( readMaybe )

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord)--, Show)

safeGet :: [a] -> Int -> Maybe a
safeGet l i | length l <= i = Nothing
            | otherwise     = Just (l !! i)

lefties = flatten
    [ [ floor (2 ** k - 1 + m) | m <- [0 .. (2 ** k) / 2 - 1] ] | k <- [1 ..] ]

leftBranch :: [Maybe a] -> [Maybe a]
leftBranch list = map
    snd
    (filter (\p -> fst p `elem` take (length list) lefties) (zip [1 ..] list))

rightBranch :: [Maybe a] -> [Maybe a]
rightBranch list = map
    snd
    (filter (\p -> fst p `notElem` take (length list) lefties) (zip [1 ..] list)
    )

listToTree :: [Maybe a] -> Tree a
listToTree []       = Empty
-- listToTree [x] = Node x Empty Empty
listToTree (x : xs) = case x of
    Nothing -> Empty
    Just v  -> Node v (listToTree (leftBranch xs)) (listToTree (rightBranch xs))

depth :: Tree a -> Int
depth t = case t of
    Empty        -> -1 -- lol
    Node _ t1 t2 -> max (depth t1) (depth t2) + 1

instance Show a => Show (Tree a) where
    show t =
        unlines (map show (linesFromCoords (coordify t)))

type Coord a = Map.Map (Int, Int) a
coordify :: Tree a -> Coord a
coordify t = case t of
    Empty -> Map.empty
    Node v t1 t2 ->
        let
            dep    = depth t
            start  = floor (2 ** (fromIntegral dep - 1))
            offset = quot start 2
            leftBranch =
                coordinification t1 Map.empty 1 (start + offset) offset (dep, 1)
            rightBranch = coordinification t2
                                           Map.empty
                                           1
                                           (start - offset)
                                           offset
                                           (dep, -1)
        in
            Map.insert (0, start) v (Map.union leftBranch rightBranch)

coordinification
    :: Tree a -> Coord a -> Int -> Int -> Int -> (Int, Int) -> Coord a
coordinification t acc d pos off fix@(last, version) = case t of
    Empty -> acc
    Node v l r ->
        let newOff        = quot off 2
            (left, right) = if d == last - 1
                then
                    ( if version == 1 then pos + 1 else pos
                    , if version == -1 then pos - 1 else pos
                    )
                else (pos + newOff, pos - newOff)
            mLeft  = coordinification l acc (d + 1) left newOff fix
            mRight = coordinification r acc (d + 1) right newOff fix
        in  Map.insert (d, pos) v (Map.union mRight mLeft)

padLeftToSize :: String -> Char -> Int -> String
padLeftToSize str c n | (length str - n) >= 0 = str
                      | otherwise             = padLeftToSize (c : str) c n

linesFromCoords :: Show a => Coord a -> [String]
linesFromCoords coords =
    let
        depth = fst . fst . Map.findMax $ coords
        rows  = floor (2 ** fromIntegral depth) + 1
        maxNodeSize =
            Map.foldl (\comp val -> (max (length (show val)) comp)) 0 coords
    in
        map
            (foldr ((++) . (" " ++)) "")
            [ [ padLeftToSize
                    (case Map.lookup (d, r) coords of
                        Nothing -> ""
                        Just v  -> show v
                    )
                    ' '
                    maxNodeSize
              | d <- [0 .. depth]
              ]
            | r <- [0 .. rows - 1]
            ]

tTsmall = listToTree [ Just k | k <- [1 .. 7] ]
tT1 = listToTree [ Just k | k <- [1 .. 31] ]
tCoords = coordinification tT1 Map.empty 0 0
-- putStr $ unlines $ map (foldr ((++) . ( " " ++)) "") lines


-- zadanie 2
-- data Either a b = Left a | Right b deriving (Show)

-- instance Functor (Either e) where
--     -- fmap :: (a -> b) -> Either e a -> Either e b
--     fmap f (Right a0) = Right $ f a0

reverseRight :: Either e [a] -> Either e [a]
-- reverseRight (Left e0) = Left e0
-- reverseRight (Right a0) = Right $ reverse a0
reverseRight = fmap reverse

class Functor f => Pointed f where
    pure :: a -> f a

instance Pointed [] where
    pure a0 = [a0]

instance Pointed Maybe where
    pure a = Just a

instance Functor Tree where
    fmap f (Node v t1 t2) = Node (f v) (fmap f t1) (fmap f t2)
    fmap f Empty = Empty

instance Pointed Tree where
    pure a = Node a Empty Empty

-- zadanie 3
maybeCharToInt :: Char -> Maybe Int
maybeCharToInt '0' = Just 0
maybeCharToInt '1' = Just 1
maybeCharToInt '2' = Just 2
maybeCharToInt '3' = Just 3
maybeCharToInt '4' = Just 4
maybeCharToInt '5' = Just 5
maybeCharToInt '6' = Just 6
maybeCharToInt '7' = Just 7
maybeCharToInt '8' = Just 8
maybeCharToInt '9' = Just 9
maybeCharToInt _   = Nothing

wordToNumber :: String -> Maybe Int
wordToNumber str = fst $ foldl
    (\(acc, k) chr ->
        ( maybeCharToInt chr >>= \x -> acc >>= \y -> return (y + 10 ^ k * x)
        , k + 1
        )
    )
    (Just 0, 0)
    (reverse str)

readInts :: String -> [Int]
readInts str = catMaybes $ map readMaybe (words str)

readInts2 :: String -> Either String [Int]
readInts2 str = foldl
    (\cur new -> case cur of
        Left  msg -> Left msg
        Right lst -> case readMaybe new of
            Nothing -> Left ("Invalid token " ++ new)
            Just x  -> Right (lst ++ [x])
    )
    (Right [])
    (words str)

sumInts :: String -> String
sumInts str = case readInts2 str of
    Left  msg  -> msg
    Right ints -> show $ sum ints

-- "dla znudzonych"
infixl 4 <*>
class Pointed f => Applicative f where
    (<*>) :: f(a->b) -> f a -> f b

instance ThirdLab.Applicative Maybe where
    (<*>) (Just f) (Just a0) = Just (f a0)
    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing

instance ThirdLab.Applicative [] where
    (<*>) (f:fs) (a0:as) = f a0 : (ThirdLab.<*>) fs as
    (<*>) [] _ = []
    (<*>) _ [] = []

-- v = Just 1
-- u = Just 2
-- f :: Int -> Int
-- f = (4 +) 
-- w = 3
-- x :: Int
-- x = 0
-- y = 0

-- tests :: [Bool]
-- tests = 
--     [ (ThirdLab.pure id ThirdLab.<*> v) == v 
--     , (ThirdLab.pure (.) ThirdLab.<*> u ThirdLab.<*> v ThirdLab.<*> w) == (u ThirdLab.<*> (v ThirdLab.<*> w) )
--     , (ThirdLab.pure f ThirdLab.<*> ThirdLab.pure x) == (ThirdLab.pure (f x) :: Maybe Int )
--     , (u ThirdLab.<*> ThirdLab.pure y) == (ThirdLab.pure ($ y) ThirdLab.<*> u )
--     ]
