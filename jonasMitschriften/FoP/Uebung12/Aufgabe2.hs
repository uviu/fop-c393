foldl f a (map g xs) = foldl _ _ xs

foldl f a (map g xs) = foldl h a xs

h p q = f p (g q)