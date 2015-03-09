module Occation where

-- | bmiTell' tells us how our BMI are
-- 
-- >>> bmiTell' 18.5
-- "You're underweight, you emo, you!"
-- 
-- >>> bmiTell' 25.0
-- "You're supposedly normal. Pffft, I bet you're ugly!"
--
-- >>> bmiTell' 30.0
-- "You're fat! Lose some weight, fatty!"
-- 
-- >>> bmiTell' 35.0
-- "You're a whale, congraturations!"
--
bmiTell' :: Double -> String
bmiTell' bmi
	| bmi <= 18.5	= "You're underweight, you emo, you!"
	| bmi <= 25.0	= "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= 30.0	= "You're fat! Lose some weight, fatty!"
	| otherwise		= "You're a whale, congraturations!"



-- | bmiTell tells us how much and how our BMI are
-- 
-- >>> bmiTell 85 1.90
-- "You're supposedly normal. Pffft, I bet you're ugly!"
-- 
-- >>> bmiTell 63 1.64
-- "You're supposedly normal. Pffft, I bet you're ugly!"
--
bmiTell :: Double -> Double -> String
bmiTell weight height
	| weight / height ^ 2 <= 18.5	= "You're underweight, you emo, you!"
	| weight / height ^ 2 <= 25.0	= "You're supposedly normal. Pffft, I bet you're ugly!"
	| weight / height ^ 2 <= 30.0	= "You're fat! Lose some weight, fatty!"
	| otherwise		= "You're a whale, congraturations!"



-- | max'returns larger value from two given values
--
-- >>> max' 10 5
-- 10
--
-- >>> max' 'a' 'z'
-- 'z'
--
max' :: (Ord a) => a -> a -> a
max' a b
	| a <= b 	= b
	| otherwise	= a



-- | myCompare Compares two given values
-- 
-- >>> 10 `myCompare` 10
-- EQ
--
-- >>> 5.7 `myCompare` 10.2
-- LT
-- 
-- >>> "cut" `myCompare`  "cat"
-- GT
--
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a == b 	= EQ
	| a < b 	= LT
	| otherwise	= GT


