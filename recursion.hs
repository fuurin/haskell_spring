module Recursion where

--　再帰の定跡
-- 基底部となる自明な解を見極める
-- 小さな部分問題に分割し、自分自身を適用



-- | 再帰でリスト内の最大要素を取り出す
-- 
-- >>> maximum' [2,5,1]
-- 5
--
-- >>> maximum' []
-- *** Exception: maximum of empty list!
--
-- 型制約、aはOrdクラス（比較可能）の値でなければいけない。
--　max 関数は、Ordクラスのインスタンスが渡されないといけない。
-- Int型なんかはそのままでいいけど、a型にはなんの属性もついていない
-- そこで、このように型制約をつける。
--
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
--
-- maxに、最終的には2つの値を渡さないといけない。
--　再帰呼び出しを繰り返し、最後にはリストが1つになる。
--　リストが1つになったら、再帰の戻りで比較を繰り返す。
-- 再帰は、行って→ストップして→戻る
--



-- | 複製
-- | 負の数にも対応するため、ガードを使う
--
-- >>> replicate' 3 5
-- [5,5,5]
--
replicate' :: Int -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' (n-1) x
--
-- x:~で、与えられたリストの先頭にxを追加



-- | リストから指定個数の要素を抜き出す
--
-- >>> take' 3 [5,4,3,2,1]
-- [5,4,3]
--
take' :: Int -> [a] -> [a]
take' n _ 
	| n <= 0	= []
take' _ []		= []
take' n (x:xs)	= x : take' (n-1) xs
--
--ガードは、値を返すときの条件を書きたいときに使える。



-- | リストを反転させる
--
-- >>> reverse' [5,4,3,2,1]
-- [1,2,3,4,5]
--
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
--
-- リストの後ろに追加するときは＋＋?
-- 追加するのはあくまでリスト？



-- | 無限リストを作る
--
-- >>>take' 5 (repeat' 3)
-- [3,3,3,3,3]
--
repeat' :: a -> [a]
repeat' x = x : repeat' x
--
--遅延評価！
--途中で切るのを忘れずに



-- | 2つのリストを綴じあわせる
--
-- >>> zip' [1,2,3] [7,8]
-- [(1,7),(2,8)]
--
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _			= []
zip' _ []			= []
zip' (x:xs) (y:ys)	= (x,y) : zip xs ys
--
--_には何でも入るので、この2つの基底部でzip [] []も対応できる。



-- | リスト内に指定された値があるか確認する
--
-- >>> 3 `elem'` [1,2,3,4,5]
-- True
--
-- >>> 6 `elem'` [1,2,3,4,5]
-- False
--
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
	| a == x = True
	| otherwise = a `elem'` xs
--
-- elemは中置換数



-- | クイックソート
--
-- >>> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- [1,2,2,3,3,4,4,5,6,7,8,9,10]
--
-- >>> quicksort "the quick brown fox jumps over the lazy dog"
-- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
--
quicksort :: (Ord a) => [a] -> [a]
quicksort [] 		= []
quicksort (x:xs)	= 
	let smallerOrEqual	= [a | a <- xs, a <= x]
	    larger      	= [a | a <- xs, a >  x]
	in quicksort smallerOrEqual ++ [x] ++ quicksort larger
--　ある条件の元、新たな集合や値を作りたいときには、let inを使う。
-- 最初に選ぶ中間値を、ピボットと呼ぶ。