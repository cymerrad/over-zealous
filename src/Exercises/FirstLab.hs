module Exercises.FirstLab where


takeOut list nth = (list !! nth, take nth list ++ drop (nth + 1) list)
takeOuts list = map (takeOut list) [0 .. length list - 1]

flatten :: [[a]] -> [a]
flatten ll = [ el | l <- ll, el <- l ]

perm :: [a] -> [[a]]
perm []   = [[]]
perm [x]  = [[x]]
perm list = flatten [ [ x : xxs | xxs <- perm xs ] | (x, xs) <- takeOuts list ]


-- jednak zrobie w tym pliku reszte tych rzeczy

-- nub == unique
nub :: (Eq a) => [a] -> [a]
nub list = reverse $ nub_rec list []
  where
    nub_rec :: (Eq a) => [a] -> [a] -> [a]
    nub_rec (x : xs) aggr
        | null xs   = if x `notElem` aggr then x : aggr else aggr
        | otherwise = nub_rec xs (if x `notElem` aggr then x : aggr else aggr)

-- wlasciwie to sort dla kilku elementow moze byc szybszy...
if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ a = a
sort3 (x, y, z) = if'
    (x <= y)
    (if' (x <= z) (if' (y <= z) (x, y, z) (x, z, y)) (z, x, y))
    (if' (y <= z) (if' (x <= z) (y, x, z) (y, z, x)) (z, y, x))


-- this below is a clusterfuck because of Int/Integer/Integral/Float misunderstanding (of mine presumably)

-- trojki pitagorejskie
range :: Integer -> [Float]
range n = [1.0 .. fromInteger n]
-- checkTriple :: Int -> Int -> Int -> Bool
checkTriple x y z = (x ** 2 + y ** 2) == z ** 2
triads :: Integer -> [(Int, Int, Int)]
triads n =
    [ (floor x, floor y, floor z)
    -- [ (x,y,z)
    | x <- range n
    , y <- range n
    , z <- range n
    , checkTriple x y z
    ]

uniqueTriads n = nub [ sort3 tri | tri <- triads n ]
