-------------
-- BST Int --
-------------

data BSTInt = EmptyT
            | BSTIntNode Int BSTInt BSTInt
            deriving Show

sumBstInt EmptyT = 0
sumBstInt (BSTIntNode val left right) = val + sumBstInt left + sumBstInt right

-------------
-- BST Int --
-------------

data BST a = EmptyBST
            | BSTNode a (BST a) (BST a)
            deriving Show

sumBst EmptyBST = 0
sumBst (BSTNode val left right) = val + sumBst left + sumBst right

---------------
-- trainings --
---------------

-- Example 1: An empty BST
emptyTree :: BST Int
emptyTree = EmptyBST

-- Example 2: A single-node BST
singleNodeTree :: BST Int
singleNodeTree = BSTNode 10 EmptyBST EmptyBST

-- Example 3: A small BST
smallTree :: BST Int
smallTree = BSTNode 10
                (BSTNode 5 EmptyBST EmptyBST)
                (BSTNode 15 EmptyBST EmptyBST)

-- Example 4: A larger BST
largerTree :: BST Int
largerTree = BSTNode 20
                (BSTNode 10
                    (BSTNode 5 EmptyBST EmptyBST)
                    (BSTNode 15 EmptyBST EmptyBST))
                (BSTNode 30
                    (BSTNode 25 EmptyBST EmptyBST)
                    (BSTNode 35 EmptyBST EmptyBST))

-- Example 5: A BST with only left children
leftSkewedTree :: BST Int
leftSkewedTree = BSTNode 10
                    (BSTNode 5
                        (BSTNode 2 EmptyBST EmptyBST)
                        EmptyBST)
                    EmptyBST

-- Example 6: A BST with only right children
rightSkewedTree :: BST Int
rightSkewedTree = BSTNode 10
                    EmptyBST
                    (BSTNode 20
                        EmptyBST
                        (BSTNode 30 EmptyBST EmptyBST))

---------------
-- functions --
---------------

depthOfBST :: BST a -> Int -- głębokość drzewa binarnego
depthOfBST EmptyBST = 0
depthOfBST (BSTNode _ left right) = 1 + max (depthOfBST left) (depthOfBST right)

flattenBSTpreorder :: BST a -> [a]
flattenBSTpreorder EmptyBST = []
flattenBSTpreorder (BSTNode val left right) = [val] ++ flattenBSTpreorder left ++ flattenBSTpreorder right

flattenBSTinorder :: BST a -> [a]
flattenBSTinorder EmptyBST = []
flattenBSTinorder (BSTNode val left right) = flattenBSTinorder left ++ [val] ++ flattenBSTinorder right

flattenBSTpostorder :: BST a -> [a]
flattenBSTpostorder EmptyBST = []
flattenBSTpostorder (BSTNode val left right) = flattenBSTpostorder left ++ flattenBSTpostorder right ++ [val]

mapBST :: (a -> b) -> BST a -> BST b -- funkcja map dla drzewa binarnego
mapBST _ EmptyBST = EmptyBST
mapBST func (BSTNode val left right) = BSTNode (func val) (mapBST func left) (mapBST func right)

insert :: Ord a => a -> BST a -> BST a -- insert element into BST
insert val EmptyBST = BSTNode val EmptyBST EmptyBST
insert val (BSTNode nodeVal left right)
    | val <= nodeVal = BSTNode nodeVal (insert val left) right
    | otherwise = BSTNode nodeVal left (insert val right)

list2BST :: Ord a => [a] -> BST a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBST
list2BST xs = resultTreeFunc xs EmptyBST
    where
        resultTreeFunc [] currTree = currTree
        resultTreeFunc (x:xs) currTree = resultTreeFunc xs (insert x currTree)

main = do
    -- BST Int test

    let tree = BSTIntNode 1 (BSTIntNode 2 EmptyT EmptyT) (BSTIntNode 3 EmptyT EmptyT)
    print tree
    print $ sumBstInt tree

    -- BST test

    let tree2 = BSTNode 1.7 (BSTNode 2.3 EmptyBST EmptyBST) (BSTNode 3.4 EmptyBST EmptyBST)
    print tree2
    print $ sumBst tree2

    print "Trainings:"

    print "1. Depth"
    print $ depthOfBST emptyTree
    print $ depthOfBST smallTree
    print $ depthOfBST largerTree
    print $ depthOfBST rightSkewedTree

    print "2. Flatten"
    print "2. 1. Preorder"
    print $ flattenBSTpreorder emptyTree
    print $ flattenBSTpreorder smallTree
    print $ flattenBSTpreorder largerTree
    print $ flattenBSTpreorder rightSkewedTree

    print "2. 2. Inorder"
    print $ flattenBSTinorder emptyTree
    print $ flattenBSTinorder smallTree
    print $ flattenBSTinorder largerTree
    print $ flattenBSTinorder rightSkewedTree

    print "2. 3. Postorder"
    print $ flattenBSTpostorder emptyTree
    print $ flattenBSTpostorder smallTree
    print $ flattenBSTpostorder largerTree
    print $ flattenBSTpostorder rightSkewedTree

    print "3. Map"
    let f = \x -> 2 * x
    print $ mapBST f emptyTree
    print $ flattenBSTinorder $ mapBST f emptyTree
    print $ mapBST f smallTree
    print $ flattenBSTinorder $ mapBST f smallTree
    print $ mapBST f largerTree
    print $ flattenBSTinorder $ mapBST f largerTree
    print $ mapBST f rightSkewedTree
    print $ flattenBSTinorder $ mapBST f rightSkewedTree

    print "4. Insert"
    let tree = EmptyBST
    let tree_cp = insert 4 tree
    let tree = tree_cp
    print tree
    print $ flattenBSTinorder tree
    let tree_cp = insert 2 tree
    let tree = tree_cp
    print tree
    print $ flattenBSTinorder tree
    let tree_cp = insert 1 tree
    let tree = tree_cp
    print tree
    print $ flattenBSTinorder tree
    let tree_cp = insert 7 tree
    let tree = tree_cp
    print tree
    print $ flattenBSTinorder tree

    print "5. List to BST"
    let valuesList = [3,4,1,5,2]
    print  valuesList
    let listToBSTObject = list2BST valuesList
    print listToBSTObject
    print $ flattenBSTinorder listToBSTObject
