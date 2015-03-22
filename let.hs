module Let where

-- | let バインド　in 返す式　というふうに使う
-- | 便利だが、ガードをまたいで使えず、局所的で可読性を損なうときがある

-- | 円柱の表面積を求める
--
-- >>> cylinder 3 4
-- 131.94689145077132
--
cylinder :: Double -> Double -> Double
cylinder r h = 
	let	sideArea = 2 * pi * r * h
		topArea = pi * r ^ 2
    in	sideArea + 2 * topArea



-- | letは式なのでどこでも使える
--
-- >>> ghci1
-- 42
--
ghci1 = 4 * (let a = 9 in a + 1) + 2



-- | ローカルスコープに関数も作れる
--
-- >>> ghci2
-- [(25,9,4)]
--
ghci2 = [let square x = x * x in (square 5, square 3, square 2)]



-- | 複数の値をバインドしたいときはセミコロンで区切る
--
-- >>> ghci3
-- (6000000,"Hey there!")
--
ghci3 = (let a = 100　; b = 200　; c = 300　in a * b * c,
		 let foo = "Hey " ; bar = "there!" in foo ++ bar)



-- | タプルを要素に変換できる
--
-- >>> ghci4
-- 600
ghci4 = (let (a,b,c)=(1,2,3) in a + b + c) * 100



-- | let式を使ったBMI
--
-- >>> calcBmis [(4, 4), (5, 5), (8, 8)]
-- [0.25,0.2,0.125]
--
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]



-- | 肥満な人のBMIのみをフィルタ
-- | (w,h) <- xs をジェネレータと呼ぶ。ジェネレータからletしたものは見えない
--
-- >>> calcBmis' [(200, 2), (500, 5), (800, 8)]
-- [50.0]
-- 
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]



-- | in を使わなければ、その後の対話で、letした関数が見える
--
-- >>> let zoot x y z = x * y + z
-- >>> zoot 3 9 2
-- 29
--
-- >>> let boot x y z = x * y + z in boot 3 4 2
-- 14
--
-- -- >>> boot
-- これは見えない
