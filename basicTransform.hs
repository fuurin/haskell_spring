module BasicTransform where

-- | リストを行列として渡すと行基本変形する関数
-- 
-- >>> basicTransform [[1,0,-1,-2],[-1,1,2,3],[2,1,-1,-3]]
-- [[1,0,-1,-2],[0,1,1,1],[0,0,0,0]]
--
basicTransform :: [[Int]] -> [[Int]]
basicTransform =