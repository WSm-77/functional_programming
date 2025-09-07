sqr :: Double -> Double
sqr x = x * x

vec3Dlen :: (Double, Double, Double) -> Double
vec3Dlen (x,y,z) = sqrt (x**2 + y**2 + z**2)

swapIntChar :: (Int, Char) -> (Char, Int)
swapIntChar (x, y) = (y, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && x == z
