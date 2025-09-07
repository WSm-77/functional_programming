import Data.List

f = (+1)

g = (*2)

h = (^3)

fg = f . g

fgh = f . g . h

sortDesc x = (reverse . sort) x

areTwoFunctionsEqAt f g [] = True
areTwoFunctionsEqAt f g (x:xs) = f x == g x && areTwoFunctionsEqAt f g xs

main = do
    let x = 2.2
    let xs = [1,5,8,2,4,3]
    print (fg x)
    print (fgh x)
