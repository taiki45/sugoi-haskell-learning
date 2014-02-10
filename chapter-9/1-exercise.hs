main :: IO ()
main = do result <- tryMatching
          while not result $ do
              putStrLn "Wrong!!"
              tryMatching
          putStrLn "Got it!!\n"
          putStrLn $ unwords $ take 13 $ repeat "* "
          putStrLn "you know L, reapers eat only apples..."

-- True is success
tryMatching :: IO Bool
tryMatching = do putStrLn "What is password?"
                 putStr "> "
                 input <- getLine
                 return $ "654321" == input

while :: (a -> Bool) -> a -> IO a -> IO a
while pred' a body
    | pred' a   = do result <- body
                     while pred' result body
    | otherwise = return a
