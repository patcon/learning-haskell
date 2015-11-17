doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber  x = if (x > 100) then x else x*2

doubleSmallNumber' x = doubleSmallNumber x + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [ 1 | _ <- xs ]

removeLowercase :: String -> String
removeLowercase string = [ char | char <- string, char `elem` ['A'..'Z'] ]

rightTriangles' maxlength perimeter = [
  (a,b,c) |
    c <- [1..maxlength],
    b <- [1..c],
    a <- [1..b],
      a^2 + b^2 == c^2,
      a+b+c == perimeter
  ]
