import System.Random

main :: IO ()
main = undefined

finiteRandom :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandom 0 g = ([], g)
--           ^ ここのパターンマッチに Eq 型クラスのインスタンスが必要
--             でも Num n だと制限が広すぎて Eq 型クラスの
--             インスタンスか否か推論できない
--             なので Int とかで Eq を制約すれば OK
--
finiteRandom n g = let (value, newGen) = random g
                       (restOfList, finalGen) = finiteRandom (n - 1) newGen
                    in (value:restOfList, finalGen)
