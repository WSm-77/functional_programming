roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a

vec2Dlen :: (Double, Double) -> Double
vec2Dlen (x,y) = sqrt (x^2 + y^2)

unit2Dvec :: (Double, Double) -> (Double, Double)
unit2Dvec (x,y) = (x',y')
    where   vec2DlenVar = vec2Dlen (x,y)
            x' = x / vec2DlenVar
            y' = y / vec2DlenVar
