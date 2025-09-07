printHello :: IO ()
printHello = putStrLn "Hello"

sqr :: Double -> Double
sqr x = x * x

vec2Dlen :: (Double, Double) -> Double
vec2Dlen (x, y) = sqrt (x^2 + y^2)

main :: IO ()
main = printHello
-- main = putStrLn(sqr 2.0)
