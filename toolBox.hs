module ToolBox where

-- | リスト全体に関数を適用する
--
-- >>> map' (+3) [1,5,3,1,6]
-- [4,8,6,4,9]
--
-- >>> map' (++ "!") ["BIFF", "BANG", "POW"]
-- ["BIFF!","BANG!","POW!"]
--
-- >>> map' (replicate 3) [3 .. 6]
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
--
-- >>> map' (map' (^2)) [[1,2],[3,4,5,6],[7,8]]
-- [[1,4],[9,16,25,36],[49,64]]
--
-- >>> map' fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- [1,3,6,2,2]
--
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs
--
--map関数はPreludeに存在するので、使えない。map'にした。



-- | 述語（ある値を渡すと真理値を返す関数）を満たすものを選びえ出す
--
-- >>> filter' (>3) [1,5,3,2,1,6,4,3,2,1]
-- [5,6,4]
--
-- >>> filter' (==3) [1,2,3,4,5]
-- [3]
--
-- >>> filter' even [1..10]
-- [2,4,6,8,10]
--
-- >>> let notNull x = not (null x) in filter' notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
-- [[1,2,3],[3,4,5],[2,2]]
--
-- >>> filter' (`elem` ['a' .. 'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
-- "uagameasadifeent"
--
-- >>> filter' (`elem` ['A'..'Z']) "u LAuGh at you bEcause u R all the same"
-- "LAGER"
--
-- >>> filter (<15) (filter even [1..20])
-- [2,4,6,8,10,12,14]
--
-- >>> [x | x <- [1..20], x < 15, even x]
-- [2,4,6,8,10,12,14]
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
	| p x = x : filter' p xs
	| otherwise = filter' p xs
-- ガードで式の値は＝を使って結ぶ
--filter関数もPreludeに存在



-- | filter' を使ってクイックソート
--
-- >>> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- [1,2,2,3,3,4,4,5,6,7,8,9,10]
--
-- >>> quicksort "the quick brown fox jumps over the lazy dog"
-- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
--
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerOrEqual = filter' (<=x) xs
	    lager = filter' (>x) xs
	in quicksort smallerOrEqual ++ [x] ++ quicksort lager
--
-- a を　Ordにするのを忘れずに
-- 集合にしてからのクイックソートを忘れずに



-- | 100000以下で、3829で割り切れる最大の数を出す関数
--
-- >>> largestDivisible
-- 99554
--
largestDivisible :: Integer
largestDivisible = head (filter' p [100000, 99999..])
	where p x = x `mod` 3829 == 0
--
-- where句は、関数を作ることもできる



-- | 10000より小さい全ての奇数の平方数の和を求める
-- | takeWhileを使う。渡された述語を満たす間リスト要素を返す
--
-- >>> takeWhile (/=' ') "elephants know how to party"
-- "elephants"
-- 
-- >>> sum (takeWhile (<10000) ( filter' odd (map (^2) [1..] ) ) )
-- 166650
--
-- >>> sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m ] )
-- 166650
-- 
--



-- | コラッツ数列
-- 
-- >>> chain 13
-- [13,40,20,10,5,16,8,4,2,1]
--
-- >>> chain 10
-- [10,5,16,8,4,2,1]
--
-- >>> chain 1
-- [1]
--
-- >>> chain 30
-- [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
--
chain :: Integer -> [Integer]
chain 1 = [1]
chain x
	| even x = x : chain (x `div` 2)
	| odd x  = x : chain (x * 3 + 1)
--
-- 1になったら終わるんじゃなくて[1]を返す
--　偶数はeven,奇数はodd



-- | 1~100までのうち、長さが15以上のコラッツ数列になる開始数がいくつあるか
-- 
-- >>> numLongChains
-- 71
--
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100] ) )
	where isLong xs = length xs >= 15



-- | map'に複数の引数を与える
--
-- >>> let listOfFuns = map' (*) [0..]
-- >>> (listOfFuns !! 4) 5
-- 20
-- !!演算子は、リストの中のｎ番目の要素を取り出す