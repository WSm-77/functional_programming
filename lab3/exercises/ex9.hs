joinElems strList = foldr (++) [] strList

main = do
    let xs = [1..3]
    let strList = ["ala", " ", "ma", " ", "kota"]
    print $ foldr (+) 0 xs
    print $ foldr (*) 1 xs
    print $ foldr (*) 0 xs
    print $ joinElems strList
