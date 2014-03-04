main :: IO ()
main = do str <- fmap reverse getLine
          putStrLn str
