module RpnSolver where

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

-- スタックと演算子の都合があえば計算
-- そうでなければ数字をスタックに積む
foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:ys) "*" = (y * x):ys
foldingFunction (x:y:ys) "+" = (y + x):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs n = read n:xs
