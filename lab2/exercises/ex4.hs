import Language.Haskell.TH (fromE)
isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s -- isPalindrome "ABBA" = True

isPalindrome2 :: [Char] -> Bool
isPalindrome2 [] = True
isPalindrome2 str = (firstLetter == lastLetter) && isPalindrome2 middle
    where
        firstLetter = head str
        lastLetter = last str
        strLen = length str
        middle = take (strLen - 2) (tail str)


capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize a = firstCapital ++ capitalize (tail a)  -- capitalize "ala" = "Ala"
    where
        firstLetter = head a
        firstCapital
            | firstLetter >= 'a' && firstLetter <= 'z' = [toEnum (fromEnum firstLetter + fromEnum 'A' - fromEnum 'a')]
            | otherwise = [firstLetter]

