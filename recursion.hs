maximum' :: (Ord a) => [a] -> a
maximun' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (head:tail) = head : take' (n - 1) tail

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (head:tail) = reverse' tail ++ [head]

repeat' :: a -> [a]
repeat' n = n : repeat' n

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smaller = quickSort [a | a <- xs, a <= x]
        bigger = quickSort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger