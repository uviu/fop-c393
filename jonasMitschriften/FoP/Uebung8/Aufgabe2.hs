data N = Z | S N deriving Show

fold:: a -> (a -> a) -> N -> a
fold z s n = case n of 
Z -> z 
S x -> s (fold z s x)

plus:: N -> N -> N 
plus x y = fold y S x

mal:: N -> N -> 
mal x y = fold Z (plus y) x

hoch:: N -> N -> N 
hoch x y = fold (S Z) (mal x) y

g:: N -> Bool
g x = fold True not x

null = Z 
eins = S null
zwei = S eins
drei = S zwei
vier = S drei