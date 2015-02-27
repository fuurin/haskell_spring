module Type where

-- | removeNonUppercase gets UPPER character from a string
-- >>> removeNonUppercase "IdontLIKEFROGS"
-- "ILIKEFROGS"
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]



-- | addThree makes a sum of three integer numbers
-- >>> addThree 1 2 3
-- 6
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z



-- | factorial makes factorial 1 to "n"
-- >>> factorial 50
-- 30414093201713378043612608166064768844377641568960512000000000000
factorial :: Integer -> Integer
factorial n = product [1..n]



-- | circumference makes a length of circumference of a circle
-- | which have "r" meters of radius in "Float"
-- >>> circumference 4.0
-- 25.132742
circumference :: Float -> Float
circumference r = 2 * pi * r



-- | circumference' makes a length of circumference of a circle
-- | which have "r" meters of radius in "Douoble"
-- >>> circumference' 4.0
-- 25.132741228718345
circumference' :: Double -> Double
circumference' r = 2 * pi * r