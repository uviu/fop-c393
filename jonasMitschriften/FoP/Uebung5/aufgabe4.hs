import Test.LeanCheck

minAdd :: Integer -> Integer -> Integer
minAdd 0 _ = 0
minAdd _ 0 = 0
minAdd x y = 1 + minAdd (x - 1) (y - 1)

maxAdd :: Integer -> Integer -> Integer
maxAdd 0 y = y
maxAdd x 0 = x
maxAdd x y = 1 + maxAdd (x - 1) (y - 1)

median :: Integer -> Integer -> Integer -> Integer
median x y z = maxAdd (minAdd x y) (maxAdd (minAdd x z) (minAdd y z))

test_minAdd :: Integer -> Integer -> Bool
test_minAdd x y = x >= 0 && y >= 0 ==> minAdd x y == min x y

test_maxAdd :: Integer -> Integer -> Bool
test_maxAdd x y = x >= 0 && y >= 0 ==> maxAdd x y == max x y

--test_maxAdd/test_minAdd prüfen mit: check test_minAdd bzw. check test_maxAdd