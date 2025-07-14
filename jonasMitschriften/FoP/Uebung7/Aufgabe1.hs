data Bool = False | True

bool:: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t 

testBool:: IO()
testBool = do

data Maybe a = Nothing | Just a deriving Show
fold:: b -> (a-> b) -> Maybe a -> b 
fold n _ Nothing = n 
fold _ j (Just x) = j x

fold n j x = case x of {Nothing -> n; (Just x1) -> j x1}
-- maybe = fold 0 (+1)
-- maybe (Just 5) 
-- ergibt: 6

data Pair a b = Pair a b deriving Show

fold f p = case p of {Pair a b -> f a b}
pair f p = fold f p 
pair (+) (Pair 2 4) 
-- ergibt: 6
pair (\ x y -> x * y) (Pair 2 4)

data Either a b = Left a | Right b deriving Show
fold l r x = case x of {Left x -> l x; Right x -> r x}
either l r x = fold l r x 
either l r x = fold (+1) (+2) 
either (Left 4)
