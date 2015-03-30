module Composition where

-- | 関数合成は、.演算子で行う
-- |後ろの関数から適用される
--
-- >>> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]
--
-- >>> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]
--
-- >>> map (\x -> negate (sum (tail x))) [[1..5],[3..6],[1..7]]
-- [-14,-15,-27]
--
-- >>> map (negate.sum.tail)　[[1..5],[3..6],[1..7]]
-- [-14,-15,-27]
--
-- >>> sum (replicate 5 (max 6.7 8.9))
-- 44.5
--
-- >>> (sum . replicate 5) (max 6.7 8.9)
-- 44.5
--
-- >>> sum .replicate 5 $ max 6.7 8.9
-- 44.5
--
-- >>> replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
-- [180,180]
--
-- >>> replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
-- [180,180]
--



-- | ポイントフリースタイル：仮引数を使わない書き方
--
-- >>> sum' [1,2,3,4,5]
-- 15
--
-- >>> sum'' [1,2,3,4,5]
-- 15
--
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0



-- | ポイントフリースタイルは、データより関数に目が行くようになり、可読性が上がることがある
--
-- >>> let fn x = ceiling (negate (tan (cos (max 50.0 x))))
-- >>> fn 60
-- 2
--
-- >>> fn 10
-- -1
--
-- >>> let fn' = ceiling . negate . tan . cos . max 50.0
-- >>> fn' 60
-- 2
--
-- >>> fn' 10
-- -1
--


-- | 関数が複雑な時は、ポイントフリースタイルがかえって可読性を悪くする
--
-- >>> oddSquareSum
-- 166650
--
-- >>> oddSquareSum'
-- 166650
--
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
-- $と.ってどう使い分ければいいんだろう…