funcList :: [Double -> Double]
funcList = [\x -> (sin x) / x, \x -> log x + sqrt x + 1, \x -> (exp 1) * x]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

evalFuncListAt' x fs = map (\f -> f x) fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

main = do
    let x = 1
    let (x_t, y_t) = (fst displEqs, snd displEqs)
    print (evalFuncListAt x funcList)
    print (evalFuncListAt' x funcList)
    print (x_t 1)
    print (y_t 1)
