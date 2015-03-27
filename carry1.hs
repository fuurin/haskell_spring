module Carry1 where

-- | Haskellでは、実は引数を1つしか取れなかった！
-- 
-- >>> max 4 5
-- 5
--
-- >>> (max 4) 5
-- 5
--


-- | どうなっていたかというと、関数が引数となって、同じ関数に適用されていた。
-- | 左から、小さな工場が作られていくイメージ
-- 
-- >>> multThree 3 5 9
-- 135
--
-- >>> ( ( (multThree 3) 5 ) 9 )
-- 135
-- 
multThree :: Int -> ( Int -> ( Int -> Int ) )
multThree x y z = x * y * z



-- | 引数が複数ある関数は、引数一つ一つに関しての関数を考えられる。
-- | つまり、新しい関数を考えることができる！
--
-- >>> let multTwoWithNine = multThree 9
-- >>> multTwoWithNine 2 3
-- 54
--



-- | 100と比較する関数
-- | compareWithHundred' 99　は、　(compare 100) 99　となるので、引数書かなくても使える！
--
-- >>> compareWithHundred 99
-- GT
--
-- >>> compareWithHundred' 99
-- GT
--
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100



-- | 中置換数でも、このような部分適用はできる。
--
-- >>> diviedeByTen 200
-- 20.0
--
-- >>> 200 / 10
-- 20.0
--
-- >>> (/10) 200
-- 20.0
--
diviedeByTen :: (Floating a) => a -> a
diviedeByTen = (/10)



-- | 大文字か小文字かを判定
--
-- >>> isUpperAlphanum 'A'
-- True
--
-- >>> isUpperAlphanum 'a'
-- False
--
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
