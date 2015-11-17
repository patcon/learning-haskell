lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName x = "Missing."

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = ((fst a + fst b), (snd a + snd b))

head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

head3 :: [a] -> [a]
head3 [] = error "empty list"
head3 [_] = error "ah!"
head3 [_,_] = error "ah!"
head3 (x:y:z:xs) = [x,y,z]

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Yo! This is an empty string!"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: RealFloat n => n -> n -> String
bmiTell weight height
  | bmi <= skinny = "Underweight!"
  | bmi <= normal = "Normal?"
  | bmi <= fat = "large"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        -- skinny = 18.5
        -- normal = 25.0
        -- fat = 30
        (skinny, normal, fat) = (18.5, 25, 30)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' :: RealFloat a => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w/h^2]

calcBmis'' :: RealFloat a => [(a, a)] -> [a]
calcBmis'' xs = [
  bmi |
    (w, h) <- xs,
      let bmi = w/h^2,
      bmi >= fat
  ]
  where fat = 25

cylinder :: (RealFloat a) => a -> a -> a
cylinder radius height =
  let sideArea = 2 * pi * radius * height
      topArea = pi * radius^2
  in  sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of
              [] -> error "No head for empty lists."
              (x:_) -> x

describeList :: Show a => [a] -> String
describeList xs = "The list is " ++ case xs of
                                      [] -> "empty."
                                      [x] -> "a singleton list."
                                      xs -> "a longer list."
