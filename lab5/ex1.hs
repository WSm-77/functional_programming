newtype IntBox = MkIntBox Int

instance Show IntBox where
    show (MkIntBox x) = show x

main = do
    putStrLn "Enter line:"
    a <- getChar
    putChar a

    line <- getLine
    putStrLn line

    let intBoxVar = MkIntBox 3
    print intBoxVar
