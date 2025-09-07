sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

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
