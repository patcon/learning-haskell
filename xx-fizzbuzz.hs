fizzbuzz :: (Integral n, Show n) => n -> String
fizzbuzz n
  | is_fizz && is_buzz = "fizzbuzz"
  | is_fizz            = "fizz"
  | is_buzz            = "buzz"
  | otherwise          = show n
  where is_fizz = (n `mod` 7) == 0
        is_buzz = (n `mod` 3) == 0
