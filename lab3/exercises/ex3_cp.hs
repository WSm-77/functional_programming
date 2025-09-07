-- Define the factorial function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Recursive approximation of exp(x)
expApproxUpTo :: Integer -> Double -> Double
expApproxUpTo 0 x = 1
expApproxUpTo n x = (x^n / fromIntegral (factorial n)) + expApproxUpTo (n - 1) x

main :: IO ()
main = do
  let x = 1
  let n = 5
  print (expApproxUpTo n x)
