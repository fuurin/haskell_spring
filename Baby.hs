module Baby where

-- | doubleMe doubles an integer number
--
-- >>> doubleMe 9
-- 18
--
-- >>> doubleMe 8.3
-- 16.6
--
-- >>> doubleUs 4 9
-- 26
--
-- >>> doubleUs 2.3 34.2
-- 73.0
--
-- >>> doubleUs 28 88 + doubleMe 123
-- 478
-- 
-- >>> doubleSmallNumber 100
-- 200
--
-- >>> doubleSmallNumber 101
-- 101
--
-- >>> doubleSmallNumber' 100
-- 201
--
-- >>> doubleSmallNumber' 101
-- 102

doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = 	if x > 100
						then x
						else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1