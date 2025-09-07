dblElems xs = map (*2) xs

sqrElems xs = [x^2 | x <- xs]


main = do
    let xs = [1..10]
    print $ dblElems xs
    print $ sqrElems xs
