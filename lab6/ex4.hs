import Control.Monad

xs2 :: [(Int,Int,Int)]
xs2 = [ (x,y,z) | let xs = [1..3], x <- xs, y <- xs, z <- xs, x > y && y > z ]

doXs2 :: [(Int,Int,Int)]
doXs2 = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  guard $ x > y && y > z
  return (x,y,z)

tryFactorial 0 = Just 1
tryFactorial n =
  if n < 0 then Nothing
  else do
    prevVal <- tryFactorial $ n - 1
    return (prevVal * n)
