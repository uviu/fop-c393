data T = A | B deriving Eq
instance Ord T where
{ B <= A = False ; _ <= _ = True }