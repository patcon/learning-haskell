binsearch_internal :: [Int] -> Int -> Int -> Int -> Int
binsearch_internal xs begin end value
  | end < begin     = -1
  | xs!!mid > value = binsearch_internal xs begin (mid-1) value
  | xs!!mid < value = binsearch_internal xs (mid+1) end value
  | otherwise       = mid
  where
  mid = begin + (div (end - begin) 2)

binsearch :: [Int] -> Int -> Int
binsearch xs value = binsearch_internal xs 0 (length xs - 1) value
