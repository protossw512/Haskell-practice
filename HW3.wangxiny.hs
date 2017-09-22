-- Unsolved:

--inTree :: Eq a => a -> Tree a-> Bool
--inTree n End = False
--inTree n (Node i l r) = i == n || inTree n r || inTree n l


--pathTo :: Eq a => a -> Tree a -> Maybe Path
--pathTo x (Node n l r)
--		| x == n = Just []
--		| inTree x l == True = Just [L] ++ pathTo x l
--		| inTree x r == True = Just [R] ++ pathTo x r
--		| otherwise = Nothing

-- don't know hot to mach data type for this part:
--		| inTree x l == True = Just [L] ++ pathTo x l
--		| inTree x r == True = Just [R] ++ pathTo x r



module HW3 where


-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)


-- | A path is just a sequence of steps. Each node in a binary tree can be
--   identified by a different path, indicating how to walk down the tree
--   starting from the root. See the examples for `valueAt`.
type Path = [Step]


-- | Binary trees with nodes labeled by an arbitrary type.
data Tree a = Node a (Tree a) (Tree a)
            | End
  deriving (Eq,Show)


-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End


-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Get the value at the node specified by a path.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt [] (Node n _ _) = Just n
valueAt _ End = Nothing
valueAt (L:xs) (Node _ l _) = valueAt xs l
valueAt (R:xs) (Node _ _ r) = valueAt xs r


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--


-- | Apply a function to the value at every node in the tree.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) (leaf 3))
--   Node False (Node True End End) (Node False End End)
--
--   >>> (mapTree not . mapTree even) (Node 5 (leaf 2) (leaf 3))
--   Node True (Node False End End) (Node True End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
mapTree f (Node m l r) = Node (f m) (mapTree f l) (mapTree f r) 
mapTree f End = End



