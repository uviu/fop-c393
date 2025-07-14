data Tree = Leaf | Branch Tree Tree deriving Show

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor a b = False

istUngerade :: Tree -> Bool
istUngerade Leaf = True
istUngerade (Branch l r) = xor(istUngerade l)(istUngerade r)

testBaum :: Tree
testBaum = Branch Leaf (Branch Leaf Leaf)

testBaum2 :: Tree
testBaum2 = Branch (Branch Leaf Leaf) (Branch Leaf Leaf)