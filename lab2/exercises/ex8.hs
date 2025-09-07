isOdd n
    | n == 1 = True
    | otherwise = isEven (n - 1)

isEven n
    | n < 0 = False
    | n == 0 = True
    | otherwise = isOdd (n - 1)
