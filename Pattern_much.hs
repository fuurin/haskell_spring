module Pattern_much where

-- | lucky says "LUCKY NUMBER SEVEN!" 
-- | when the given number is 7
--
-- >>> lucky 7
--"LUCKY NUMBER SEVEN!"
--
-- >>> lucky 4
-- "Sorry, you're out of luck, pal!"
--
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"



-- | sayMe says specific numbers 
-- | when the given number is between 1 to 5
--
-- >>> sayMe 1
-- "One!"
--
-- >>> sayMe 5
-- "Five!"
--
-- >>> sayMe 10
-- "Not between 1 and 5"
--
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"



-- | factorial makes factorial of the given number
-- | by means of recursive
--
-- >>> factorial 5
-- 120
--
-- >>> factorial 0
-- 1
-- 
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)


-- | charName says specific name 
-- | when the given character is between 'a' to 'c'
--
-- >>> charName 'a'
-- "Albert"
--
-- >>> charName 'b'
-- "Breoseph"
--
-- >>> charName 'h'
-- Of course, the exception will thrown
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Breoseph"