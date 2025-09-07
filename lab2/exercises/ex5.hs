import Language.Haskell.TH.Lib (prim)
pythagorianTriplets :: [(Integer, Integer, Integer)]
pythagorianTriplets = [(x, y, z) | x <- [1..100], y <- [x..100], z <- [y..100], x^2 + y^2 == z^2]

howManyPythagorianTriplets :: Int
howManyPythagorianTriplets = length pythagorianTriplets

isPrime :: Integral a => a -> Bool
isPrime n = null [i | i <- [2..n-1], n `mod` i == 0]

primesInInterval :: Int -> [Int]
primesInInterval n = [i | i <- [2..n], null (divs i)]
    where
        divs val = [i | i <- 2 : [3,5..toEnum (round (sqrt (fromIntegral val)))], val `mod` i == 0]


howManyPrimesInInterval :: Int -> Int
howManyPrimesInInterval n = length (primesInInterval n)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = null xs || (x == head xs && allEqual xs)

