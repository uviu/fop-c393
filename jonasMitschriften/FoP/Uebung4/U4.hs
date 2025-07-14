import qualified Prelude
data Bool = False | True deriving Prelude.Show

-- Negation
not :: Bool -> Bool
not True = False
not False = True

-- Konjunktion
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

-- Antivalenz
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
--xor True True = False
--xor False False = False
--xor a b = False
xor _ _ = False

-- Implikation
imp :: Bool -> Bool -> Bool
imp True False = False
imp a b = True

-- Majority
maj :: Bool -> Bool -> Bool -> Bool
maj _ True True = True
maj True _ True = True
maj True True _ = True
--maj _ _ _ = False
maj a b c = False

--maj2 :: Bool -> Bool -> Bool -> Bool
--maj2 a b c = case a of
--True -> case b of
--    True -> True
--    False -> case c of
--        True -> True
--        False -> False
--False -> case b of
--    False -> False
--    True -> case c of
--        False -> False
--        True -> True

maj3 :: Bool -> Bool -> Bool -> Bool
maj3 a b c = or (and a b) (or (and a c) (and b c))