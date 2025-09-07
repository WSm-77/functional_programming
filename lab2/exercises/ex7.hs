{-# LANGUAGE BangPatterns #-}

sum' :: Num t => [t] -> t
sum' lst = loop 0 lst
    where
        loop res [] = res
        loop res (x:xs) = x + (loop res xs)

product' :: Num a => [a] -> a
product' [] = 0
product' lst = loop 1 lst
    where
        loop res [a] = res * a
        loop res (x:xs) =  loop (res * x) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
    where
        loop !acc [] = acc
        loop !acc (x:xs) = loop (x + acc) xs
