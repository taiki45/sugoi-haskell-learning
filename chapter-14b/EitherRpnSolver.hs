module RpnSolver where

import Control.Monad

type Error = Either String

solveRPN :: String -> Error Double
solveRPN = fmap head . foldM foldingFunction [] . words

-- スタックと演算子の都合があえば計算
-- そうでなければ数字をスタックに積む
foldingFunction :: [Double] -> String -> Error [Double]
foldingFunction (x:y:ys) "*" = return $ (y * x):ys
foldingFunction (x:y:ys) "+" = return $ (y + x):ys
foldingFunction (x:y:ys) "-" = return $ (y - x):ys
foldingFunction xs s = case reads s of
                           (n,_):_ -> return $ n:xs
                           _      -> Left $ "can't match with given string: " ++ s
