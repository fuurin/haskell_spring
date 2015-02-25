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