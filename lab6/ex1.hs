(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Nothing to extract!!!"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe x = Just x

-- (>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- x >^$> f = (extractMaybe x) >$> f

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >^$> _ = Nothing
(Just x) >^$> f = f x

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

-- (>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> g $ extractMaybe $ f x

-- (>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> insertMaybe x >^$> f >^$> g

(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>> g = \x -> fmap (extractMaybe . g) (f x)

doSafeTail3x :: [a] -> Maybe [a]
doSafeTail3x xs = do
    t1 <- safeTail xs
    t2 <- safeTail t1
    t3 <- safeTail t2
    return t3

doSafeTail3x' :: [a] -> Maybe [a]
doSafeTail3x' xs = safeTail xs >>= safeTail >>= safeTail

main = do
    x <- getLine >>= \line -> return ("Variable " ++ line)
    putStrLn x
    print "End"
