module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
    (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
    (===) = (==)

instance Mon (Reg c) where
    m1 = Eps -- tests superimpose this on me and I disagree - IMHO: Empty
    x <> Empty = x
    Empty <> y = y
    x <> y = x :| y
  
simpl :: Reg c -> Reg c
simpl x = x

nullable :: Reg c -> Bool
nullable x = case x of
    Eps -> True
    Lit c -> False
    Empty -> False
    Many y -> True
    y :> z -> nullable y && nullable z
    y :| z -> nullable y || nullable z

empty :: Reg c -> Bool 
empty r = case r of
    Empty -> True
    _ -> False

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

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
