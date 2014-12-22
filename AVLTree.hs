----------------------------------------------------------------------------
--- AVLTree.hs
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--- Module exports
----------------------------------------------------------------------------
module AVLTree (AVLTree,
                emptyAVL,
				treeMember,
				treeInsert,
				treeDelete,
				treeBuild,
				inorder,
				treeSort)
				where

treeMember  :: (Ord a,Show a) => a -> AVLTree a -> Bool
treeInsert  :: (Ord a,Show a) => a -> AVLTree a -> AVLTree a
treeDelete  :: (Ord a,Show a) => a -> AVLTree a -> AVLTree a
treeBuild   :: (Ord a,Show a) => [a] -> AVLTree a
inorder     :: (Ord a,Show a) => AVLTree a -> [a]
treeSort    :: (Ord a,Show a) => [a] -> [a]
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- Tree definition
----------------------------------------------------------------------------
data (Ord a,Show a) => AVLTree a = EmptyAVL | NodeAVL a (AVLTree a) (AVLTree a) deriving Show
emptyAVL = EmptyAVL
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeMember
--- Check if element exists in tree
----------------------------------------------------------------------------
treeMember v' EmptyAVL = False
treeMember v' (NodeAVL v left right)
    | v' == v   = True  
    | v' < v    = treeMember v' left
    | otherwise = treeMember v' right
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeInsert
--- Insert an element into a tree
----------------------------------------------------------------------------
treeInsert v' EmptyAVL = NodeAVL v' EmptyAVL EmptyAVL
treeInsert v' (NodeAVL v left right)
    | v' < v = fixLeftHeavy (NodeAVL v (treeInsert v' left) right)
    | v' > v = fixRightHeavy (NodeAVL v left (treeInsert v' right))
    | otherwise = (NodeAVL v left right)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- fixLeftHeavy
--- Balance a left heavy tree
----------------------------------------------------------------------------
fixLeftHeavy :: (Ord a, Show a) => AVLTree a -> AVLTree a

fixLeftHeavy at@(NodeAVL a bt@(NodeAVL b bl br) ar)
    | bh - arh < 2 = NodeAVL a bt ar
    | height bl < height br = rotateLeftRight at
    | otherwise = rotateRight at
    where arh = height ar
          bh  = height bt
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- fixRightHeavy
--- Balance a right heavy tree
----------------------------------------------------------------------------
fixRightHeavy :: (Ord a, Show a) => AVLTree a -> AVLTree a

fixRightHeavy at@(NodeAVL a al bt@(NodeAVL b bl br))
    | bh - alh < 2 = NodeAVL a al bt
    | height bl > height br = rotateRightLeft at
    | otherwise = rotateLeft at
    where alh = height al
          bh = height bt
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- rotateRightLeft
--- Perform a right left rotation
rotateRightLeft :: (Ord a,Show a) => AVLTree a -> AVLTree a

rotateRightLeft (NodeAVL v left right) = rotateLeft (NodeAVL v left (rotateRight right))
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- rotateLeftRight
--- Perform a left right rotation
----------------------------------------------------------------------------
rotateLeftRight :: (Ord a,Show a) => AVLTree a -> AVLTree a

rotateLeftRight (NodeAVL v left right) = rotateRight (NodeAVL v (rotateLeft left) right)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- rotateLeft
--- Perform a left rotation
----------------------------------------------------------------------------
rotateLeft :: (Ord a,Show a) => AVLTree a -> AVLTree a

rotateLeft (NodeAVL v left (NodeAVL right rightLeft rightRight)) = (NodeAVL right (NodeAVL v left rightLeft) rightRight)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- rotateRight
--- Perform a right rotation
----------------------------------------------------------------------------
rotateRight :: (Ord a,Show a) => AVLTree a -> AVLTree a

rotateRight (NodeAVL v (NodeAVL left leftLeft leftRight) right) = (NodeAVL left leftLeft (NodeAVL v leftRight right))
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeDelete
--- Delete an element from a tree
----------------------------------------------------------------------------
treeDelete v' EmptyAVL = EmptyAVL 
treeDelete v' (NodeAVL v left EmptyAVL)
    | v' == v = left
treeDelete v' (NodeAVL v EmptyAVL right)
    | v' == v = right
treeDelete v' (NodeAVL v left right)
    | v' < v  = balanceTree(NodeAVL v(treeDelete v' left)right)
    | v' > v  = balanceTree(NodeAVL v left(treeDelete v' right))
    | v' == v = balanceTree(NodeAVL minRight left (treeDelete minRight right))
    where minRight = minTree right
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- balanceTree
--- Balance a tree
----------------------------------------------------------------------------
balanceTree :: (Ord a, Show a) => AVLTree a -> AVLTree a

balanceTree EmptyAVL = EmptyAVL
balanceTree (NodeAVL v left right)
    | lefth+1 < righth = fixRightHeavy(NodeAVL v left right)
    | lefth > righth+1 = fixLeftHeavy(NodeAVL v left right)
    | otherwise        = NodeAVL v left right
    where lefth = height left
          righth = height right
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- height
--- Get the height of a tree
----------------------------------------------------------------------------
height :: (Ord a,Show a) => AVLTree a -> Int

height EmptyAVL          = 0
height (NodeAVL v left right) = 1 + max (height left) (height right)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- minTree
--- Find the smallest element in a tree
----------------------------------------------------------------------------
minTree (NodeAVL v EmptyAVL EmptyAVL) = v
minTree (NodeAVL v left EmptyAVL)     = min v (minTree left)
minTree (NodeAVL v EmptyAVL right)    = min v (minTree right)
minTree (NodeAVL v left right)        = min v (min (minTree left)(minTree right))
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeBuild
--- Builds a tree from a list
----------------------------------------------------------------------------
treeBuild [] = EmptyAVL
treeBuild vs = foldr treeInsert EmptyAVL vs
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- inorder
--- Get elements of a tree in order
----------------------------------------------------------------------------
inorder EmptyAVL                      = []
inorder (NodeAVL v EmptyAVL EmptyAVL) = [v]
inorder (NodeAVL v left EmptyAVL)     = inorder left ++ [v]
inorder (NodeAVL v EmptyAVL right)    = [v] ++ inorder right
inorder (NodeAVL v left right)        = inorder left ++ [v] ++ inorder right
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeSort
--- Sort a list using AVLTree
----------------------------------------------------------------------------
treeSort [] = []
treeSort list = inorder (treeBuild list)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- Test trees
----------------------------------------------------------------------------
t1::AVLTree Integer
t1= NodeAVL 3 (NodeAVL 2 (NodeAVL 1 EmptyAVL EmptyAVL) EmptyAVL) EmptyAVL

t2::AVLTree Integer
t2= NodeAVL 1 EmptyAVL (NodeAVL 2 EmptyAVL (NodeAVL 3 EmptyAVL EmptyAVL)) 

t3::AVLTree Integer
t3= NodeAVL 1 EmptyAVL (NodeAVL 3 (NodeAVL 2 EmptyAVL EmptyAVL) EmptyAVL)

t4::AVLTree Integer
t4= NodeAVL 3 (NodeAVL 1 EmptyAVL (NodeAVL 2 EmptyAVL EmptyAVL)) EmptyAVL

t5::AVLTree Integer
t5= NodeAVL 6 (NodeAVL 4 (NodeAVL 2 (NodeAVL 1 EmptyAVL EmptyAVL) (NodeAVL 3 EmptyAVL EmptyAVL)) (NodeAVL 5 EmptyAVL EmptyAVL)) (NodeAVL 8 (NodeAVL 7 EmptyAVL EmptyAVL) (NodeAVL 9 EmptyAVL EmptyAVL))
----------------------------------------------------------------------------