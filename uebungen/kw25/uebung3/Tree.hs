module Tree
  ( Tree(..)
  , foldTree
  , countLeaves
  , countBranches
  , sumKeys
  , depth
  , maximumKey
  , keyAt
  ) where

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b

foldTree leafF _       (Leaf x)     = leafF x
foldTree leafF branchF (Branch l r) =
  let l' = foldTree leafF branchF l
      r' = foldTree leafF branchF r
  in  branchF l' r'

countLeaves :: Tree a -> Int
countLeaves = foldTree (const 1) (+)

countBranches :: Tree a -> Int
countBranches = foldTree (const 0) (\l r -> 1 + l + r)

sumKeys :: Num a => Tree a -> a
sumKeys = foldTree id (+)

depth :: Tree a -> Int
depth = foldTree (const 1) (\l r -> 1 + max l r)

maximumKey :: Ord a => Tree a -> a
maximumKey = foldTree id max

keyAt :: [Int] -> Tree a -> Maybe a
keyAt []     _             = Nothing
keyAt (0:ps) (Leaf x)      = Just x
keyAt (1:ps) (Leaf _)      = Nothing
keyAt (0:ps) (Branch l _)  = keyAt ps l
keyAt (1:ps) (Branch _ r)  = keyAt ps r
