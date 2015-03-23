module Recursion where

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



-- | 複製関数
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


