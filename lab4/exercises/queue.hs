module MyQueue (
    MyQueue
    , empty
    , isEmpty
    , put
    , get
) where

empty :: MyQueue a
isEmpty :: MyQueue a -> Bool
put :: a -> MyQueue a -> MyQueue a
get :: MyQueue a -> (a, MyQueue a)

newtype MyQueue a = MkMyQueue [a] deriving Show

empty = MkMyQueue []
isEmpty (MkMyQueue lst) = null lst
put val (MkMyQueue lst) = MkMyQueue (lst ++ [val])
get (MkMyQueue (x:xs)) = (x, MkMyQueue xs)
