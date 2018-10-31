module BinaryTreeExercises where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f z =
   case f z of
        Nothing        -> Leaf
        Just (l, n, r) -> Node (unfold f l) n (unfold f r)

-- e.g.
-- treeBuild 0 -> Leaf
-- treeBuild 1 -> Node Leaf 0 Leaf
-- treeBuild 2 -> Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
-- treeBuild 3 -> Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))
--             OR
--                Node (Node (Node Leaf 2 Leaf)
--                           1
--                           (Node Leaf 2 Leaf))
--                     0
--                           (Node (Node Leaf 2 Leaf)
--                           1
--                           (Node Leaf 2 Leaf))
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x >= n then Nothing else Just(x+1, x, x+1)) 0

-- e.g.
--               0
--              / \
--             1   1
-- OR
--               0
--              / \
--             1   1
--            / \ / \
--           2  2 2  2
