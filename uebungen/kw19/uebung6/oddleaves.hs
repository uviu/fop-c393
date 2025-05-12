-- gegebener datentyp
data Tree = Leaf | Branch Tree Tree deriving (Show, Eq)

-- ung. anz. blätter prädikat
oddleaves :: Tree -> Bool
oddleaves Leaf = True
oddleaves (Branch l r) = oddleaves l /= oddleaves r

