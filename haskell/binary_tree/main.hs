module Main where


data BinaryTree a = 
  Empty | 
  Node a (BinaryTree a) (BinaryTree a) 
  deriving Show

treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node a left right) = 1 + max (treeHeight left) (treeHeight right)

isBalanced :: BinaryTree a -> Bool
isBalanced Empty = True
isBalanced (Node a left right) = (abs ((treeHeight left) - (treeHeight right)) <= 1 && (isBalanced left) && (isBalanced right))


main :: IO ()
main = do
  isBalanced Empty
  isBalanced (Node 10 (Node 10 Node 10 Empty) (Node 10 Node 10 Empty))
