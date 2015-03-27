module Carry2 where

-- | Haskellは、関数を引数にしたり、関数を返り値にできる。
-- | 実演としての、渡された関数を2回適用する関数
--
-- >>> applyTwice (+3) 10
-- 16
--
-- >>> applyTwice　(++ "HAHA ") "HEY "
-- "HEY HAHA HAHA "
--
-- >>> applyTwice ("HAHA " ++) "HEY "
-- "HAHA HAHA HEY "
--
-- >>> let multThree x y z = x * y * z
-- >>> applyTwice (multThree 2 2) 9
-- 144
--
-- >>> applyTwice (3:) [1]
-- [3,3,1]
--
--
--　型の最初の()は必須！
--             　関数　　　返り値1  返り値2
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)



-- | 関数とリスト2つを渡して、関数を適用した1つのリストにする関数
--
-- >>> zipWith' (+) [4,2,5,6] [2,6,2,3]
-- [6,8,7,9]
--
-- >>> zipWith' max [6,3,2,1] [7,3,1,5]
-- [7,3,2,5]
-- 
-- >>> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
-- ["foo fighters","bar hoppers","baz aldrin"]
--
-- >>> zipWith' (*) (replicate 5 2) [1..]
-- [2,4,6,8,10]
--
-- >>> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]
--
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys
--
-- 再帰の基底部に使うリストは、_が要素ある方で、[]が要素ない方
-- 基底部に渡す引数fは、基底部では使わないのでブランクカードにして良い



-- | 渡した2つの引数の順番を逆にして、渡された関数に渡す関数
--
-- >>> zip [1,2,3,4,5] "hello"
-- [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
--
-- >>> flip' zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]
--
-- >>> zipWith' div [2,2..] [10,8,6,4,2]
-- [0,0,0,0,1]
--
-- >>> flip' (zipWith' div) [2,2..] [10,8,6,4,2]
-- [5,4,3,2,1]
--
flip' :: ( a -> b -> c) -> b -> a -> c
flip' f a b = f b a
--
--　型の順番が変わることに注意！
-- あくまで関数は一つしか渡せないので(zipWith' div)とする