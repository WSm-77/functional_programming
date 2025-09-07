data MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt val1) (MkMyInt val2) = val1 == val2

instance Ord MyInt where
    (<=) (MkMyInt val1) (MkMyInt val2) = val1 <= val2

---------------
-- trainings --
---------------

data Fraction a = MkFraction {nom :: a, denom :: a}

instance (Eq a, Num a) => Eq (Fraction a) where
    (==) (MkFraction {nom = nom1, denom = denom1}) (MkFraction {nom = nom2, denom = denom2}) = nom1 * denom2 == nom1 * denom2

instance (Ord a, Num a) => Ord (Fraction a) where
    compare (MkFraction {nom = nom1, denom = denom1}) (MkFraction {nom = nom2, denom = denom2}) = compare (nom1 * denom2) (nom2 * denom1)

instance Show a => Show (Fraction a) where
    show (MkFraction {nom = nom1, denom = denom1}) = show nom1 ++ "/" ++ show denom1

main = do
    -- Eq test
    putStrLn "Eq test"
    putStrLn ""
    print $ MkMyInt 1 == MkMyInt 1
    print $ MkMyInt 7 == MkMyInt (-3)
    print $ MkMyInt 7 /= MkMyInt (-3)   -- works

    -- Ge test
    putStrLn ""
    putStrLn "Ge test"
    putStrLn ""
    print $ MkMyInt 1 <= MkMyInt 1
    print $ MkMyInt 1 <= MkMyInt 3
    print $ MkMyInt 1 <= MkMyInt (-3)
    print $ MkMyInt 1 < MkMyInt (-3)    -- works

    -- trainings
    let f1 = MkFraction 1 2
    let f2 = MkFraction 2 4
    let f3 = MkFraction 1 5
    putStrLn ""
    putStrLn "1. Eq"
    putStrLn ""
    print $ f1 == f2
    print $ f1 < f3
    print $ f1 > f3
    print f1
    print f3
