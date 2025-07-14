check $ \ (x::Ordering) (y:: Ordering) (z::Ordering) -> (x <> y) <> z == x <> (y <> z)
check $ \ (x:: Ordering) -> mempty <> x == x && x <> mempty == x
check $ \ (x::Ordering) (y::Ordering) -> x <> == y <> x

data Pair
instance (Eq a, Eq b) => Eq (Pair a b) where
(Pair px py) == (Pair qx qy) = px == qx && py = qx

instance (Ord a, Ord b) => Ord (Pair a b) where
compare (Pair px py) (Pair qx qy)=
case compare px qx of
Eq -> compare py qy 
other -> other 