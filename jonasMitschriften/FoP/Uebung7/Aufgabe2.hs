sum L = case L of 
    Nil -> 0
    x:xs -> (+) x (sum xs) 

len L = case L of
    Nil -> 0
    x:xs -> (+) 1 (leng xs)

prod L = case L of {Nil -> 1; x:xs -> x * (prod xs)}

fold nil cons L = case L of 
    [] -> nil
    x:xs -> cons x (fold nil cons xs)

sum L = fold 0 (+) L 
prod L = fold 1 (*) L
len L = fold 0 (\ x xs -> 1 + xs) L

-- =====================

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

count t = case t of {Leaf a -> 1; Branch a b -> (+) (count a) (count b)}
sumKeys t = case t of {Leaf a -> a; Branch a b -> (+) (sumKeys a) (sumKeys b)}

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b 
count = foldTree (const 1) (+)
sumKeys = foldTree