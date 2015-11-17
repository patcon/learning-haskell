removeLowercase :: String -> String
removeLowercase string = [ char | char <- string, char `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Integer -> Integer -> Integer -> Integer
addThree' x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

circumference'' r = 2 * pi * r

emptyTuple x = ()


