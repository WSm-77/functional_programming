roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
 let d = sqrt (b * b - 4 * a * c)
     e = 2 * a
 in ( (-b - d) / e, (-b + d) / e )

unit2Dvec :: (Double, Double) -> (Double, Double)
unit2Dvec (x,y) =
    let vec2Dlen = sqrt (x^2 + y^2)
        x' = x / vec2Dlen
        y' = y / vec2Dlen
    in (x',y')
