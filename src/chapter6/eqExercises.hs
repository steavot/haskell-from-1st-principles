-- Eq instance exercises
-- Write Eq typeclass instance for these types.

-- 1.
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn m) (TisAn n) = (==) m n

-- 2.
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (Two m n) == (Two m' n') = m == n && m' == n'

-- 3.
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = (==) x x'
  (==) (TisAString xs) (TisAString xs') = (==) xs xs'
  (==) _ _ = False

-- 4.
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (==) x x' && (==) y y'

-- 5.
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (==) a a' && (==) b b'

-- 6.
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = (==) a a'
  (==) (ThatOne a) (ThatOne a') = (==) a a'
  (==) (ThatOne a) (ThisOne a') = (==) a a'
  (==) (ThisOne a) (ThatOne a') = (==) a a'

-- 7.
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello p) (Hello p') = (==) p p'
  (==) (Goodbye p) (Goodbye p') = (==) p p'
  (==) _ _ = False

