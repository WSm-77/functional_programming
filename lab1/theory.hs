-- funkcje polimorficzne
g ::a -> a
g a = a

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

-- -- klasy numeryczne
-- class Num a where
--     (+) :: (a, a) -> a

-- instance Num Complex where
--     x + y = ...

-- -- klasy porównanwcze
-- class Eq a where
--     (==) :: (a, a) -> Bool

-- instance Eq Complex where
--     x == y = ...

increment :: Num a => a -> a
increment x = x + 1

-- haskel sam dedukuje typy zmiennych
-- f :: (Ord a, Num a) => (Bool, Bool, a, a) -> Bool
f (x,y,z,w) = if x < y then z + w < 3 else x


