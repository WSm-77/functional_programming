import Text.XHtml (p, pre)
sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

factorial x = product [1..x]

expApproxUpTo 0 = \x -> 1
expApproxUpTo n = \x -> (x^n / fromIntegral (factorial n)) + expApproxUpTo (n - 1) x

dfc f h = \x -> ((f (x + h)) - f (x)) / h

d2fc f h = \x -> (df (x + h) - df x) / h
  where
    df = dfc f h

main = do
  let x = 1
  let n = 10
  let val = expApproxUpTo n x
  let f = \x -> x ^ 2
  let df = dfc f 0.00001
  let d2f = d2fc f 0.00001
  print (f x)
  print (df x)
  print (2 * x)
  print (d2f x)
  print (2)
  print val

