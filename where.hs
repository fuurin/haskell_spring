module Where where

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
	| bmi <= skinny	= "You're underweight, you emo, you!"
	| bmi <= normal	= "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= fat	= "You're fat! Lose some weight, fatty!"
	| otherwise		= "You're a whale, congraturations!"
	where	bmi = weight / height ^ 2
        	skinny = 18.5
        	normal = 25.0
        	fat = 30.0
-- whereの後ろのインデントの個数をそろえる。見た目がそろっててもダメ。スペース4つ×2とタブ1



-- whereは、関数の本体側に影響を与えることはできない。↓動かない例
--greet :: String -> String
--greet "Juan" = niceGreeting ++ " Juan!"
--greet "Fernando" = niceGreeting ++ " Fernando!"
--greet name = badGreeting ++ " " ++ name
--	where	niceGreeting = "Hello! So very nice to see you,"
--			badGreeting = "Oh! Pfft. It's you."



-- | greet greets nicely when the name is known, otherwise badly
--
-- >>> greet "Juan"
-- "Hello! So very nice to see you, Juan!"
--
-- >>> greet "Fernando"
-- "Hello! So very nice to see you, Fernando!"
--
-- >>> greet "Shusei"
-- "Oh! Pfft. It's you. Shusei"
--
badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name



-- | bmiTell' tells us how much and how our BMI are
-- 
-- >>> bmiTell' 85 1.90
-- "You're supposedly normal. Pffft, I bet you're ugly!"
-- 
-- >>> bmiTell' 63 1.64
-- "You're supposedly normal. Pffft, I bet you're ugly!"
--
bmiTell' :: Double -> Double -> String
bmiTell' weight height
	| bmi <= skinny	= "You're underweight, you emo, you!"
	| bmi <= normal	= "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= fat	= "You're fat! Lose some weight, fatty!"
	| otherwise		= "You're a whale, congraturations!"
	where	bmi = weight / height ^ 2
        	(skinny, normal, fat) = (18.5, 25.0, 30.0)



-- | initials makes initial from given full name
--
-- >>> initials "Shusei" "Komatsu"
-- "S. K."
--
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where	(f:_) = firstname
        	(l:_) = lastname


-- | calcBmis makes a BMI list from the list of pairs of weight and height
--
-- >>> calcBmis [(4, 4), (5, 5), (8, 8)]
-- [0.25,0.2,0.125]
--
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
	where 	bmi w h = w / h ^ 2
