import Data.List
import Data.Char

capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

formatStr str = foldr1 (\x y -> x ++ " " ++ y) $
    map capitalize $
    filter ((>1) . length) $
    words str

example =
            sum .
            takeWhile (<=400) .
            filter odd .
            map (^2) $
            [1..]

main = do
    let sentence = "tomasz  t  ,     bogdan anna . Jerzy j    maria"
    print $ capitalize sentence
    print $ words sentence
    print $ formatStr sentence
    print example
