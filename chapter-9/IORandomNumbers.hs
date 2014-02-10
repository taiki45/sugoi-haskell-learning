import Control.Monad
import System.Random

main :: IO ()
main = do gen <- getStdGen
          print $ take 10 $ (randomRs (0, 99) gen :: [Int])
