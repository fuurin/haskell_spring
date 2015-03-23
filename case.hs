module Case where

-- | case式とはこんな感じで動く
--
-- >>> head' [1,2,3]
-- 1
--
-- >>> head' []
-- *** Exception: No head for empty lists!
--
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x



-- | case式　case ～変数について of ～だったら -> 結果
-- | あてはまるものがなければランタイムエラー
--
-- >>> head'' [1,2,3]
-- 1
--
-- >>> head'' []
-- *** Exception: No head for empty lists!
--
head'' :: [a] -> a
head'' xs =	case xs of	[] -> error "No head for empty lists!"
	                	(x:_) -> x
-- ブロック記述は、全行のタブの数をそろえる



-- | 式の途中でパターンマッチできる。
--
-- >>> describeList []
-- "The list is empty."
--
-- >>> describeList ['a']
-- "The list is a singleton list."
--
-- >>> describeList [1,2,3]
-- "The list is a longer list."
--
describeList :: [a] -> String
describeList ls = "The list is " ++
				case ls of [] -> "empty."
				           [x] -> "a singleton list."
				           xs -> "a longer list."



-- | whereを使って、what関数を定義し、caseっぽい動きをさせる
--
-- >>> describeList' []
-- "The list is empty."
--
-- >>> describeList' ['a']
-- "The list is a singleton list."
--
-- >>> describeList' [1,2,3]
-- "The list is a longer list."
--
describeList' :: [a] -> String
describeList' ls = "The list is " ++　what ls
				where what [] = "empty."
				      what [x] = "a singleton list."
				      what xs = "a longer list."
