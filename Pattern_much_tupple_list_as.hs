module Pattern_much_tupple_list_as where

-- | addVectors makes sum of two given vectors 
--
-- >>> addVectors (1, 2) (3, 4)
--(4.0,6.0)
--
-- >>> addVectors (1.2, 2.3) (3.4, 4.5)
--(4.6,6.8)
--
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double) 
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



-- | first, second, third returns element of triple vector
--
-- >>> first (1, "two", 3.0)
-- 1
--
-- >>> second (1, "two", 3.0)
-- "two"
--
-- >>> third (1, "two", 3.0)
-- 3.0
--
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z



-- | head' returns head of given list
-- | if the given list is empty, head' returns error
-- 
-- >>> head' [4,5,6]
-- 4
--
-- >>> head' "Hello"
--'H'
--
-- >>> head' []
-- *** Exception: Can't call head on an empty list, dummy!
--
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x



-- | tell tells various information about given list
-- 
-- >>> tell [1]
-- "The list has one element: 1"
--
-- >>> tell [True, False]
-- "The list has two elements: True and False"
--
-- >>> tell [1,2,3,4]
-- "This list is long. The first two elements are: 1 and 2"
--
-- >>> tell []
-- "The list is empty"
--
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++show y



-- | badAdd makes only sum of triple list
-- >>> badAdd [1,2,3]
-- 6
--
-- >>> badAdd [100,20]
-- Of course, it returns an exception
--
badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z



-- | firstletter returns first letter of given String
-- | by using as pattern
-- 
-- >>> firstletter "Dracula"
-- "The first letter of Dracula is D"
--
-- >>> firstletter ""
-- "Empty string, whoops!"
--
firstletter :: String -> String
firstletter "" = "Empty string, whoops!"
firstletter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]