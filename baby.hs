doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Real a) => a -> a
doubleSmallNumber x = if x < 100
    then x * 2
    else x

boomBang :: (Integral a) => [a] -> [String]
boomBang xs = [ if x < 10 then "BOOM" else "BANG" | x <- xs, odd x ]

nouns = ["hobo", "frog", "pope"]
adjectives = ["scheming", "angry", "rose-cheeked"]

combination :: [String]
combination = [ adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' :: (Num a) => [b] -> a
length' xs = sum [ 1 | _ <- xs]

triangles :: Int -> Int -> [(Int, Int, Int)]
triangles size perimeter = [ (x,y,z) |
    z <- [1..size], y <- [1..z], x <- [1..y],
    z^2 == y^2 + x^2,
    x + y + z == perimeter]
