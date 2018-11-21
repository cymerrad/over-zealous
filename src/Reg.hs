module Reg where

data Reg c
    = Lit c           -- jeden znak 
    | Reg c :> Reg c  -- konkatenacja
    | Reg c :| Reg c  -- alternatywa (suma)
  
    | Many (Reg c)    -- gwiazdka 
    | Eps             -- slowo puste
    | Empty           -- jezyk pusty
    deriving (Eq,Show)   

size :: Reg c -> Int
size (x :> y) = size x + size y + 1
size (x :| y) = size x + size y + 1
size (Many x) = 1 + size x
size _ = 1