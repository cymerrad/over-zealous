module ThirdLabExpr where

data Exp
    = EInt Integer                         -- stała całkowita             
    | EAdd Exp Exp                 -- e1 + e2
    | ESub Exp Exp                 -- e1 - e2
    | EMul Exp Exp                 -- e1 * e2
    | EVar String                    -- zmienna
    | ELet String Exp Exp    -- let var = e1 in e2 
    deriving (Eq, Show)

instance Num Exp where
    -- (+) :: a -> a -> a
    -- (-) :: a -> a -> a
    -- (*) :: a -> a -> a
    -- negate :: a -> a
    -- abs :: a -> a
    -- signum :: a -> a
    -- fromInteger :: Integer -> a
    (+) x y = EAdd x y
    (-) x y = ESub x y
    (*) x y = EMul x y
    abs x = undefined
    signum x = undefined
    fromInteger x = EInt x

testExprs :: [Exp]
testExprs =
    [ EMul (EInt 5)                    (EMul (EVar "t") (EVar "t"))
    , EMul (EAdd (EInt (-2)) (EInt 2)) (EVar "x")
    ]

simpl :: Exp -> Exp
simpl x = case x of
    EMul (EInt 0) y        -> EInt 0
    EMul y        (EInt 0) -> EInt 0
    EMul (EVar y) (EInt 1) -> EVar y
    EMul (EInt 1) (EVar y) -> EVar y
    EMul y        z        -> EMul (simpl y) (simpl z)

    EAdd (EInt 0) y        -> simpl y
    EAdd y        (EInt 0) -> simpl y
    EAdd (EInt y) (EInt z) -> EInt (y + z)
    EAdd (EVar w) (EVar u) -> if w == u then EMul 2 (EVar w) else x
    EAdd y        z        -> EAdd (simpl y) (simpl z)

    ESub (EInt y) (EInt z) -> EInt (y - z)
    ESub y        z        -> ESub (simpl y) (simpl z)

    ELet w y z             -> ELet w (simpl y) (simpl z)

    _                      -> x


deriv :: String -> Exp -> Exp
deriv var exp = case exp of
    EAdd y z -> EAdd (deriv var y) (deriv var z)
    ESub y z -> ESub (deriv var y) (deriv var z)
    EMul y z -> EAdd (EMul (deriv var y) z) (EMul y (deriv var z))

    EVar w   -> if w == var then EInt 1 else EVar w
    EInt y   -> EInt 0

    _        -> exp
