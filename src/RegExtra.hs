module RegExtra where
import           Mon
import           Reg
import           Data.List

-- data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
    (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
    (===) = (==)

-- instance (Eq c) => Eq (Reg c) where
--     (==) x1 x2 = case (x1, x2) of
--         (Lit a, Lit b) -> a == b
--         (x11 :| x12, x21 :| x22) -> ((x11 == x21) && (x12 == x22)) || ((x11 == x22) && (x12 == x21))
--         (_, _) -> False

-- this is a syntactic monoid
instance Mon (Reg c) where
    m1 = Eps
    x <> Eps = x
    Eps <> y = y
    x <> y = x :> y

-- case by case I suppose
simpl :: (Eq c) => Reg c -> Reg c
simpl (x1 :> x2) = case (x1, x2) of
    (x11 :> x12, x21 :> x22) -> simpl $ x11 :> (x12 :> (x21 :> x22))
    (x11 :> x12, _         ) -> simpl $ x11 :> (x12 :> x2)
    (x11 :| x12, x21 :| x22) ->
        simpl (x11 :> x21) :| (x11 :> x22) :| (x12 :> x21) :| (x21 :> x22)
    (x11 :| x12, _         ) -> simpl $ (x11 :> x2) :| (x12 :> x2)
    (_         , x21 :| x22) -> simpl $ (x1 :> x21) :| (x1 :> x22)
    (_         , _         ) -> simpl x1 :> simpl x2

simpl (x1 :| x2) = case (x1, x2) of
    (x11 :| x12, x21 :| x22) ->
        if ((x11 == x21) && (x12 == x22)) || ((x11 == x22) && (x12 == x21))
            then x1
            else simpl x1 :| simpl x2
    (_, _) -> simpl x1 :| simpl x2

simpl (Lit  x) = Lit x
simpl (Many x) = Many (simpl x)
simpl Eps      = Eps
simpl Empty    = Empty


nullable :: Reg c -> Bool
nullable x = case x of
    Eps    -> True
    Lit c  -> False
    Empty  -> False
    Many y -> True
    y :> z -> nullable y && nullable z
    y :| z -> nullable y || nullable z

empty :: Reg c -> Bool
empty r = case r of
    Empty -> True
    _     -> False

der :: c -> Reg c -> Reg c
der c r = r

ders :: Eq c => [c] -> Reg c -> Reg c
ders c r = r

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = False

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = False

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a' .. 'z'] :| alts ['A' .. 'Z']
digit = alts ['0' .. '9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
