
qSort [] = []
qSort lst = qSort left ++ [pivot] ++ qSort right
    where
        pivot = last lst
        left = [x | x <- lst, x < pivot]
        right = [x | x <- lst, x > pivot]

mergeSort [] = []
mergeSort [a] = [a]
mergeSort lst = merge (mergeSort left) (mergeSort right)
    where
        listLen = length lst
        leftLen = round(fromIntegral(listLen) / 2)
        left = take leftLen lst
        right = drop leftLen lst
        merge [] [] = []
        merge ar1 [] = ar1
        merge [] ar2 = ar2
        merge (x:xs) (y:ys) = if x < y
                              then [x] ++ merge xs ([y] ++ ys)
                              else [y] ++ merge ([x] ++ xs) ys
