module Exercises.SecondLab where

indexOf :: Char -> String -> Maybe Int
indexOf c str = tailRec str 0
  where
    tailRec "" _ = Nothing
    tailRec (h : rest) pos | h == c    = Just pos
                           | otherwise = tailRec rest (pos + 1)

positions :: Char -> String -> [Int]
positions c str = tailRec str [] 0
  where
    tailRec :: String -> [Int] -> Int -> [Int]
    tailRec "" acc _ = acc
    tailRec rest acc off = -- hacking much? 'rest' of the word, 'acc'umulator, 'pos'ition of c in str, 'off'set
        let pos = indexOf c rest
        in
            case pos of
                Nothing  -> acc
                Just val -> tailRec (drop (val + 1) rest)
                                    ((val + off) : acc)
                                    (off + val + 1)

inits list = map ((\f -> f list) . take) [0 .. length list]

incAll :: [[Int]] -> [[Int]]
incAll ll = [ [ succ el | el <- inside ] | inside <- ll ]

nubWFilter :: (Eq a) => [a] -> [a]
nubWFilter list = tailRecObvi list []
  where
    tailRecObvi [] acc = acc
    tailRecObvi (x : xs) acc
        | null xs   = x : acc
        | otherwise = tailRecObvi (filter (x /=) xs) (x : acc)
