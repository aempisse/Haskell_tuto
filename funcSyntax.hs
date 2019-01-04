bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder radius height =
    let sideArea = 2 * pi * radius * height
        topArea = pi * radius ^ 2
    in  sideArea + 2 * topArea

headPatternMatch :: [a] -> a
headPatternMatch [] = error "No head for empty lists!"
headPatternMatch (x:_) = x

headCaseExpression :: [a] -> a
headCaseExpression xs = case xs of [] -> error "No head for empty lists!"
                                   (x:_) -> x

describeList :: [a] -> String
describeList xs = case xs of [] -> "This is an empty list"
                             [x] -> "This is a singleton list"
                             xs -> "This is a long list"