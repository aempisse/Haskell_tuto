zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

quickSort'' :: (Ord a) => [a] -> [a]
quickSort'' [] = []
quickSort'' (x:xs) =
    let smaller = quickSort'' (filter (>= x) xs)
        bigger = quickSort'' (filter (< x) xs)
    in smaller ++ [x] ++ bigger

largestDivisible :: (Integral a) => a -> a
largestDivisible divisor = head (filter predicate [100000, 99999..])
    where predicate x = x `mod` divisor == 0

sumOfAllOddSquaresSmallerThan :: (Integral a) => a -> a
sumOfAllOddSquaresSmallerThan n = sum (takeWhile (<= n) (filter odd (map (^2) [1..])))

collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence n
    | even n = n : collatzSequence (n `div` 2)
    | odd n  = n : collatzSequence (n * 3 + 1)

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
exerciceCollatzSequence :: Int
exerciceCollatzSequence = length (filter greaterThan (map collatzSequence [1..100]))
    where greaterThan xs = length xs > 15
