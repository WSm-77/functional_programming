main  = do
    let x = 5 :: Int
    let y = 2 :: Int
    let f1 = \x -> x - 2
    let f2 = \x y -> sqrt(fromIntegral (x^2 + y^2))
    let divideByThree = \x -> fromIntegral (x) / 3
    let id = \x -> x
    let f7 = \x -> (mod x 2) == 0
    print (f2 x y)
    print (divideByThree x)
    print (id x)
    print (f7 x)
