plus :: N -> N -> N
plus Z y = y
plus (S x) y = S (plus x y)

mult :: N -> N -> N
mult Z _ = Z 
mult (S x) y = plus y (mult x y)

pow :: N -> N -> N
pow _ Z = S Z --x^0 = 1
pow x (S y) = mult x (pow x y)

instance Listable N where
    tiers = cons0 Z \/ cons1 S

mult_com :: N -> N -> Bool
mult_com x y = mult x y == mult y x

mult_ass :: N -> N -> Bool
mult_ass x y z = mult x (mult y z) == mult (mult x y) z

mult_dis :: N -> N -> Bool
mult_dis x y z = 

mult_null :: N -> N -> Bool
mult_null x = mult Z x == Z && mult x

pow_null :: N -> N -> Bool
pow_null x = 

pow_eins :: N -> N -> Bool
pow_eins x =