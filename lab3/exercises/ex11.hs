concat' [] = []
concat' (x:xs) = x ++ concat' xs

main = do
    let strArr = ["ala", "ma", "kota"]
    let x = [[1,2], [3,4], [5,6]]
    let y = [[[1,2], [3,4]], [[5,6], [1,2,3,4]]]
    print $ concat' strArr
    print $ concat' x
    print $ concat' y
    print $ (concat' . concat') y
    print $ concatMap (\x -> x ++ "!") ["Ready", "Steady", "Go"]
