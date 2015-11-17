-- TODO: Find out why so slow, and make faster
fibonacci :: Integral n => n -> n
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

max' :: Ord a => [a] -> a
max' [] = error "maximum of empty list"
max' [x] = x
max' (head:tail)
  | head > maxTail = head
  | otherwise   = maxTail
  where maxTail = max' tail

max'' :: (Ord a) => [a] -> a
max'' [] = error "maximum of empty list"
max'' [x] = x
max'' (x:xs) = max x (max'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- doesn't work
take'' :: (Num i, Ord i) => i -> [a] -> [a]
take'' n arr@(x:xs)
  | n <= 0          = [] -- edge condition
  | length arr == 0 = [] -- edge condition (fails!)
  | otherwise       = x : take'' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' n x = take' n (repeat' x)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
