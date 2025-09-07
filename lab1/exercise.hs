vec3Dlen :: (Double, Double, Double) -> Double
vec3Dlen (x,y,z) = sqrt (x**2 + y**2 + z**2)

swapIntChar :: (Int, Char) -> (Char, Int)
swapIntChar (x, y) = (y, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && x == z

sgn :: Double -> Int
sgn x = if x > 0
        then 1
        else if x == 0
        then 0
        else -1

absInt :: Int -> Int
absInt x = if x >= 0
        then x
        else -x

toUpper :: Char -> Char
toUpper ch
        | ch >= 'a' && ch <= 'z' = toEnum (fromEnum ch + fromEnum 'A' - fromEnum 'a')
        | otherwise = ch

isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'

romanDigit :: Char -> String
romanDigit ch
        | ch == '1' = "I"
        | ch == '2' = "II"
        | ch == '3' = "III"
        | ch == '4' = "IV"
        | ch == '5' = "V"
        | ch == '6' = "VI"
        | ch == '7' = "VII"
        | ch == '8' = "VIII"
        | ch == '9' = "IX"
        | otherwise = ""

-- funkcja main
main :: IO ()
main = do
    putStrLn "vec3Dlen\n"

    print (vec3Dlen (3.0, 4.0, 12.0))

    putStrLn "\nswapIntChar\n"

    let x = 5
    let y = 'c'
    print (x, y)
    print (swapIntChar (x, y))

    putStrLn "\nthreeEqual\n"

    print (threeEqual (1,1,1))
    print (threeEqual (1,1,2))

    putStrLn "\nsgn\n"

    print (sgn 1.2)
    print (sgn (-2.1))
    print (sgn 0.0)

    putStrLn "\nabs\n"

    print (abs 1)
    print (abs (-2))
    print (abs 0)

    putStrLn "\ntoUpper\n"

    print (toUpper 'b')
    print (toUpper 'C')

    putStrLn "\nisDigit\n"

    print (isDigit '7')
    print (isDigit 'b')

    putStrLn "\nromanDigit\n"

    print (romanDigit '7')
    print (romanDigit '5')
