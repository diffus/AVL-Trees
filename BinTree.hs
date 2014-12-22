----------------------------------------------------------------------------
--- BinTree.hs
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--- Module exports
----------------------------------------------------------------------------

module BinTree (BinTree,
                emptyTree,
				treeMember,
				treeInsert,
				treeDelete,
				treeBuild,
				inorder,
				treeSort)
				where

treeMember   :: (Ord a,Show a) => a -> BinTree a -> Bool
treeInsert   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
treeDelete   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
treeBuild    :: (Ord a,Show a) => [a] -> BinTree a
inorder      :: (Ord a,Show a) => BinTree a -> [a]
treeSort     :: (Ord a,Show a) => [a] -> [a]
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- Tree definition
----------------------------------------------------------------------------
data (Ord a) => BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving Show
emptyTree = EmptyBT
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeMember
--- Check if element exists in tree
----------------------------------------------------------------------------
treeMember v' EmptyBT = False
treeMember v' (NodeBT v left right)
    | v==v'     = True
	| v'<v      = treeMember v' right
	| otherwise = treeMember v' right
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeInsert
--- Insert an element into a tree
----------------------------------------------------------------------------
treeInsert v' EmptyBT = NodeBT v' EmptyBT EmptyBT
treeInsert v' (NodeBT v left right)
    | v'==v     = NodeBT v left right
	| v'<v      = NodeBT v (treeInsert v' left) right
	| otherwise = NodeBT v left (treeInsert v' right)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeBuild
--- Builds a tree from a list
----------------------------------------------------------------------------
treeBuild vs = foldr treeInsert EmptyBT vs
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- treeDelete
--- Delete an element from a tree
----------------------------------------------------------------------------
treeDelete v' EmptyBT = EmptyBT 
treeDelete v' (NodeBT v left EmptyBT)
    | v'==v = left
treeDelete v' (NodeBT v EmptyBT right)
    | v'==v = right
treeDelete v' (NodeBT v left right)
    | v'<v  = NodeBT v (treeDelete v' left) right
    | v'>v  = NodeBT v left (treeDelete v' right) 
    | v'==v = NodeBT b left (treeDelete b right)
    where b = minTree right
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- minTree
--- Find the smallest element in a tree
----------------------------------------------------------------------------
minTree (NodeBT v EmptyBT EmptyBT) = v
minTree (NodeBT v left EmptyBT)    = min v (minTree left)
minTree (NodeBT v EmptyBT right)   = min v (minTree right)
minTree (NodeBT v left right)      = min v (min (minTree left)(minTree right))
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- inorder
--- Get elements of a tree in order
----------------------------------------------------------------------------
inorder EmptyBT                    = []
inorder (NodeBT v EmptyBT EmptyBT) = [v]
inorder (NodeBT v left EmptyBT)      = inorder left ++ [v]
inorder (NodeBT v EmptyBT right)      = [v] ++ inorder right
inorder (NodeBT v left right)           = inorder left ++ [v] ++ inorder right
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--- treeSort
--- Sort a list using AVLTree
----------------------------------------------------------------------------
treeSort list = inorder (treeBuild list)
----------------------------------------------------------------------------


----------------------------------------------------------------------------
--- Test trees
----------------------------------------------------------------------------
t1:: BinTree Integer
t1= NodeBT 5 (NodeBT 2 EmptyBT EmptyBT) EmptyBT

t2:: BinTree Integer
t2= NodeBT 5 (NodeBT 2 EmptyBT (NodeBT 4 (NodeBT 3 EmptyBT EmptyBT) EmptyBT)) EmptyBT

t3::BinTree Integer
t3= NodeBT 5 (NodeBT 2 EmptyBT (NodeBT 4 (NodeBT 3 EmptyBT EmptyBT) EmptyBT)) (NodeBT 6 EmptyBT (NodeBT 7 EmptyBT (NodeBT 10 (NodeBT 9 EmptyBT EmptyBT) (NodeBT 11 EmptyBT EmptyBT))))
