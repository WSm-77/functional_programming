import Data.Complex (polar)
type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

main = do
    let (x1,y1) = polarToCartesian' (1,pi/4)
    let (x2,y2) = polarToCartesian' (x1,y1)
    let cartCoord1 = (1, 1)
    let polarCoord1 = (2, pi/4)
    let cartCoord2 = polarToCartesian'' $ MkPolarCoord'' (1, pi/4)
    -- let (x2,y2) = polarToCartesian'' (x1,y1)
    print cartCoord1
    print polarCoord1
    print $ polarToCartesian' (x1, x2)
    -- print cartCoord2 -- error: No instance for (Show (CartesianCoord'' Double))
