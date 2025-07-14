op :: Ordering -> Ordering -> Ordering
op x y = case x of {LT -> LT; EQ -> y; GT -> GT}

--check $ \ (x::Ordering) (y::Ordering) -> op x y == op y x
--check $ \ (x::Ordering) (y::Ordering) (z::Ordering) -> op x (op y z) == op (op x y) z
--check $ \ (x::Ordering) -> op x LT == x für rechtsneutral (LT, GT, EQ)
--check $ \ (x::Ordering) -> op LT x == x für linksneutral (LT, GT, EQ)
--check $ \ (x::Ordering) -> (x <> x) == x