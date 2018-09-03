isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

myAbs :: Integer -> Integer
myAbs x = if positive then x else -x
  where positive = x >= 0

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

