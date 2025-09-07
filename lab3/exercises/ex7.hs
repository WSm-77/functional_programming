import Data.Char

myodd x = x `mod` 2 == 1

onlyOdd [] = []
onlyOdd (x:xs)
    | odd' x = x : onlyOdd xs
    | otherwise = onlyOdd xs
    where
        odd' x = x `mod` 2 == 1

onlyUpper [] = []
onlyUpper (x:xs)
    | isUpper x = x : onlyUpper xs
    | otherwise = onlyUpper xs

onlyUpper' [] = []
onlyUpper' (x:xs)
    | isUpper' x = x : onlyUpper' xs
    | otherwise = onlyUpper' xs
    where
        -- asciiVal x = fromEnum x
        isUpper' x = 'A' <= x && x <= 'Z'

main = do
    let xs = [1..10]
    let sentence = "My name is Inigo Montoya. You killed my father. Prepare to die."
    print $ onlyOdd xs
    print $ filter myodd xs
    print $ onlyUpper' sentence
