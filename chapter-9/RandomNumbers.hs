import Control.Monad
import System.Random

main :: IO ()
main = print $ take 10 numbers
    where numbers :: [Int]
          numbers = randomRs (1, 100) (mkStdGen 1)
