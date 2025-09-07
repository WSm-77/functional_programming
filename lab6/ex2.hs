f x y z = x + y + z

safeTail [] = Left "Empty list"
safeTail (x:xs) = Right xs

safeDiv x y
    | y == 0 = Left "Can not divide by 0"
    | otherwise = Right $ x / y

main = do
    print $ f <$> (Just 5) <*> (Just 3) <*> (Just 2)

    print "End"
