fromBits[True, False, False, True, False]

foldl(\p q -> p * 2 + case q of {False -> 0;True -> 1}) 0 [True, False, False, True, False]

foldr(\p q -> case p of {False -> case q of {2 -> 2; 0 -> 0};True -> case q of{2 -> 18; 0 -> 2}}) 0 [True, False, False, True, False]
foldr(\p q -> case p of {False -> q ;True -> case q of{2 -> 18; 0 -> 2}}) 0 [True, False, False, True, False]