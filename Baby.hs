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

doubleMe x = x + x

doubleUs x y = x * 2 + y * 2