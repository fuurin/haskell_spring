module Fold where

-- | 左からリストを消化していく、左畳み込み
--
-- >>> sum' [3,5,2,1]
-- 11
--
-- >>> sum'' [3,5,2,1]
-- 11
--
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
-- カリー化バージョン、sum''は、右辺に置き換わると考える。



-- | 逆に、右畳み込み(に見える）。MAP関数を実装
--
-- >>> map' (+3) [1,2,3]
-- [4,5,6]
--
-- >>> map'' (+3) [1,2,3]
-- [4,5,6]
--
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
--foldは、関数、原点、リスト

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
--foldlを使って無理やり作る。しかし、＋＋関数は：関数より遅いので、普通リストを扱うときはfoldr



-- | elem関数。値が見つかれば、その時点からTrueになる。
--
-- >>> elem' 1 [1,2,3]
-- True
-- 
-- >>> elem' 4 [1,2,3]
-- False
--
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc ) False ys



-- | foldl1:左畳み込みで、与えられたリストの末尾を原点（アキュムレータ）とする
-- 
-- >>> maximum' [10,20,3]
-- 20
--
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max
-- | foldl1,foldr1は、空リストが渡されたときランタイムえら0になるので注意



-- | reverse関数
--
-- >>> reverse' [1,2,3,4,5]
-- [5,4,3,2,1]
--
-- >>> reverse'' [1,2,3,4,5]
-- [5,4,3,2,1]
--
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []



-- | product関数
-- 
-- >>> product' [1,2,3]
-- 6
--
product' :: (Num a) => [a] -> a
product' = foldl (*) 1



-- |　filter関数
--
-- >>> filter' (>3) [1,3,5]
-- [5]
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []
-- 「xがpを満たす」は、p x



-- | last関数
-- 
-- >>> last' [1,2,3,4,5]
-- 5
--
last' :: [a] -> a
last' = foldl1 (\_ x -> x)
--foldは、リストの最後まで見ないと気が済まないので、foldrで止める、ってことはできない。



-- | and関数、全部TrueならTrue、そうでなければFalse
--
-- >>> and' [True,True,True]
-- True
--
-- >>> and' [True,False,True]
-- False
--
-- >>> and' (repeat False)
-- False
--
and' :: [Bool] -> Bool
and' = foldr (&&) True
-- foldで無限大リストがあつかえる。&&関数は、Falseが渡されるとその時点で終了する。



-- | scan関数、アキュムレータの移り変わりがリストで分かる！
-- | 移り変わる方向は、lrによる
--
-- >>> scanl (+) 0 [3,5,2,1]
-- [0,3,8,10,11]
--
-- >>> scanr (+) 0 [3,5,2,1]
-- [11,8,3,1,0]
--
-- >>> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
-- [3,4,5,5,7,9,9,9]
-- 
-- >>> scanl (flip (:)) [] [3,2,1]
-- [[],[3],[2,3],[1,2,3]]
--



-- | 自然数の平方根を小さいものから足して行ったとき、1000を超えるのは何個め？
--
-- >>> sqrtSums
-- 131
--
-- >>> sum (map sqrt [1..130])
-- 993.6486803921487
-- 
-- >>> sum (map sqrt [1..131])
-- 1005.0942035344083
--
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
--takeWhile,Boolの間、リストから取る。



-- | ＄関数、関数適用演算子
-- | 優先順位が低いので、関数の()をなくすのに使える
--
-- >>> sum (map sqrt [1,4,9])
-- 6.0
--
-- >>> sum $ map sqrt [1,4,9]
-- 6.0
--
-- >>> sum (filter (>10) (map (*2) [2..10]))
-- 80
--
-- >>> sum $ filter (>10) $ map (*2) [2..10]
-- 80
-- 
-- >>> map ($ 3) [(+4),(10*),(^2),sqrt]
-- [7.0,30.0,9.0,1.7320508075688772]