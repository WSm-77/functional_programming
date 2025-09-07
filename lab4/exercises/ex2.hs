import Control.Arrow (ArrowChoice(left))
--------------------------------------------
-- product type example (one constructor) --
--------------------------------------------

-- Tylko inty
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

-- dowolny typ
data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

-----------------------------------------
-- sum type example (two constructors) --
-----------------------------------------

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

--------------------------------------------------
-- enum type example (special case of sum type) --
--------------------------------------------------

data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

-----------------------------------------------------
-- data type with constructors that take arguments --
-----------------------------------------------------

data Shape = Circle Float
            | Rectangle Float Float

calcArea :: Shape -> Float
calcArea (Circle r) = pi * r^2
calcArea (Rectangle a b) = a * b

---------------
-- trainings --
---------------

data TrafficLight = RedLight | YellowLight | GreenLight

actionFor :: TrafficLight -> String
actionFor RedLight = "Wait"
actionFor YellowLight = "Prepare to drive"
actionFor GreenLight = "Go!!!"

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

getRootVal (Node val _ _) = val
getRootVal (EmptyT) = error "Empty tree"

main = do
    -- product type
    let p24 = MkCart2DVec'' {x = 2, y = 4}
    let p35 = MkCart2DVec'' {y = 5, x = 3}
    print $ x p24
    print $ y p24
    print $ xCoord'' p35
    print $ yCoord'' p35

    -- sum type
    let myList = Cons 1 $ Cons 2 EmptyL
    print myList
    print $ head' myList

    -- enum type
    let color = Red
    print $ leadingActor color

    -- Shape test
    let circle1 = Circle 2.0
    let rect1 = Rectangle 3 4
    print $ calcArea circle1
    print $ calcArea rect1

    -- Lights test
    print $ actionFor RedLight
    print $ actionFor YellowLight
    print $ actionFor GreenLight

    -- Tree test
    let tree = Node 4 (Node 2 (Node 1 EmptyT EmptyT) (Node 3 EmptyT EmptyT)) (Node 5 EmptyT EmptyT)
    print tree
    print $ getRootVal tree
