import Control.Monad (forever)
import Data.Char (toUpper)

-- with bind operator (p.286)
main = forever $ getLine >>= putStrLn . (map toUpper)
   -- (forever
   --   ((>>=) getLine
   --          ((.) putStrLn (map toUpper))))

{-
main = getContents >>= putStrLn . (map toUpper)
-}
