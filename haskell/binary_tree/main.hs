module Main where


data BinaryTree a = 
  Empty 
  | Node Int (BinaryTree a) (BinaryTree a) 
  deriving Show

treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node value left right) = 1 + max (treeHeight left) (treeHeight right)

isBalanced :: BinaryTree a -> Bool
isBalanced Empty = True
isBalanced (Node value left right) = (abs ((treeHeight left) - (treeHeight right)) <= 1 && (isBalanced left) && (isBalanced right))

searchValue :: BinaryTree a -> Int -> Bool
searchValue Empty searched = False
searchValue (Node value left right) searched =
  | value == searched = True
  | otherwise = (searchValue left searched) || (searchValue right searched) || False


main :: IO ()
main = do
  isBalanced Empty
  isBalanced (Node 10 (Node 10 Node 10 Empty) (Node 10 Node 10 Empty))
  searchValue (Node 10 (Node 20 Node 30 Empty) (Node 35 Node 12 Empty)) 20
