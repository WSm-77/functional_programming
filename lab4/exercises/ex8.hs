module Stack (
    Stack
    , empty
    , isEmpty
    , push
    , top
    , pop
) where

empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a, Stack a)

newtype Stack a = MkStack [a] deriving Show

empty = MkStack []
isEmpty (MkStack lst) = null lst
push val (MkStack lst) = MkStack (lst ++  [val])
top (MkStack lst) = last lst
pop (MkStack (x:xs)) = (x, MkStack xs)

