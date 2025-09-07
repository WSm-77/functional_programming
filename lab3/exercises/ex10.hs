isSortedAsc [] = True
isSortedAsc xs = and (zipWith (<) xs (tail xs))

everySecond xs = map snd $ filter (even . fst) $ zip [0..] xs

main = do
    let nums = [1..5]
    let vals = [2,2,2,2,2]
    let chars = ['a'..'e']
    let endlessList = [1..]
    let ones = 1 : ones
    print $ zip [1..5] ["a", "b"]
    print $ zipWith (,) nums chars
    print $ zipWith (*) nums vals
    print $ take 5 (zip endlessList (tail endlessList))
    print $ everySecond nums
    print $ everySecond chars
    print $ take 5 ones
