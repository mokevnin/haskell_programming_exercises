{- sum -}
sum' [] = 0
sum' (a:x) = a + sum x

{- create -}
replicate' m n
  | m <= 0 = []
  | otherwise = n : replicate' (m - 1) n

{-toke-}
take' n _
  | n <= 0 = []
take' n [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' x = x : repeat' x

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' a [] = False
elem' a (x:xs)
  | a == x = True
elem' a (x:xs) = elem a xs

sort' [] = []
sort' (x:xs) = (sort' [a | a <- xs, a <= x]) ++ [x] ++ (sort' [a | a <- xs, a > x])

applyTwice f x = f (f x)

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = [f x y] : zipWith' f xs ys

flip' f x y = f y x
