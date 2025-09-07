sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

length' = sumWith (\x -> 1)

main :: IO ()
main = do
    let x = [1..5]
    let id = \x -> x
    let f = \x -> x^2
    print (sumWith id x)
    print (sumWith f (take 3 x))
