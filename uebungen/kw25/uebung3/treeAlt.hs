-- Datentypdefinition

module TreeFold where

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Show, Eq)

-- Allgemeines Rekursionsschema (fold)

foldTree
  :: (a -> b)        -- Funktion für Leaf
  -> (b -> b -> b)   -- Funktion für Branch
  -> Tree a
  -> b
foldTree fLeaf fBranch t = case t of
  Leaf x      -> fLeaf x
  Branch l r  -> fBranch (foldTree fLeaf fBranch l)
                              (foldTree fLeaf fBranch r)

-- Beispiel-Funktionen auf Bäumen

-- 1) Anzahl der Blätter
countLeaves :: Tree a -> Int
countLeaves = foldTree (const 1) (+)

-- 2) Anzahl der Verzweigungsknoten
countBranches :: Tree a -> Int
countBranches = foldTree (const 0) (\l r -> 1 + l + r)

-- 3) Summe der Schlüssel
sumKeys :: Num a => Tree a -> a
sumKeys = foldTree id (+)

-- 4) Tiefe des Baumes
depth :: Tree a -> Int
depth = foldTree (const 0) (\l r -> 1 + max l r)

-- 5) Größter Schlüssel
maxKey :: Ord a => Tree a -> a
maxKey = foldTree id max

-- 6) Linkster Schlüssel (linkes unterstes Blatt)
leftMost :: Tree a -> a
leftMost = foldTree id (\l _ -> l)

-- Beispielbaum zum Testen
exampleTree :: Tree Int
exampleTree = Branch (Branch (Leaf 3) (Leaf 7)) (Leaf 5)

-- Beispiel-Ausgaben 
-- countLeaves exampleTree      -- 3
-- countBranches exampleTree    -- 2
-- sumKeys exampleTree          -- 15
-- depth exampleTree            -- 2
-- maxKey exampleTree           -- 7
-- leftMost exampleTree         -- 3
