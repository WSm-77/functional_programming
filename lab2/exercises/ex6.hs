fib :: Int -> Int
fib n = case n of
    1 -> 0
    2 -> 1
    _ -> fib (n - 1) + fib (n - 2)

fib2 :: Int -> Int
fib2 n = fibs !! n
    where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

length' :: Num a1 => [a2] -> a1
length' [] = 0
length' (x:xs) = 1 + length' xs
