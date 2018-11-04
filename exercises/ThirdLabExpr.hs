module ThirdLabExpr where

data Exp 
  = EInt Integer             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2 
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

testExp2 :: Exp
testExp2 = (2 + 2) * 3

simpl :: Exp -> Exp
simpl x = case x of
  EMul y z -> if sy == EInt 0 || sz == EInt 0 then EInt 0 else 
    if sy == EInt 1 then sz else
      if sz == EInt 1 then sy else
        EMul sy sz
        where
          (sy, sz) = (simpl y, simpl z)
  EAdd y z -> if sy == EInt 0 then sz else
    if sz == EInt 0 then sy else
      EAdd sy sz
      where
        (sy, sz) = (simpl y, simpl z)
  _ -> x
  
simpl2 :: Exp -> Exp
simpl2 x = case x of
  EMul (EInt 0) y -> EInt 0
  EMul y (EInt 0) -> EInt 0
  EMul (EVar y) (EInt 1) -> EVar y
  EMul (EInt 1) (EVar y) -> EVar y
  EAdd (EInt 0) y -> simpl y
  EAdd y (EInt 0) -> simpl y

  _ -> x

testExp3 :: Exp
testExp3 = (2 - (2 + 0)) * 3