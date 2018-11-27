module Reg where

-- helpers for examples
import           Text.ParserCombinators.Parsec
                                         hiding ( Empty
                                                , digit
                                                , letter
                                                )

data Reg c
    = Lit c           -- jeden znak 
    | Reg c :> Reg c  -- konkatenacja
    | Reg c :| Reg c  -- alternatywa (suma)

    | Many (Reg c)    -- gwiazdka 
    | Eps             -- slowo puste
    | Empty           -- jezyk pusty
    deriving (Show, Eq)

size :: Reg c -> Int
size (x :> y) = size x + size y + 1
size (x :| y) = size x + size y + 1
size (Many x) = 1 + size x
size _        = 1


-- parsing
data AB = A | B deriving(Eq,Ord,Show)
parseGroup, parseA, parseB, parseLit, parseConc, parseAlt :: Parser (Reg AB)
parseA = do
    oneOf "aA"
    return (Lit A)
parseB = do
    oneOf "bB"
    return (Lit B)
parseLit = parseA <|> parseB
parseGroup = do
    char '('
    x <- try parseAlt <|> parseConc
    char ')'
    return x
parseConc = do
    left  <- try parseGroup <|> parseLit
    right <- try parseGroup <|> parseLit
    return (left :> right)
parseAlt = do
    left <- try parseGroup <|> parseLit
    char '|'
    right <- try parseGroup <|> parseLit
    return (left :| right)

readExpr :: String -> Reg AB
readExpr input = case parse parseGroup "what is this for?" input of
    Left  err -> Empty
    Right res -> res

