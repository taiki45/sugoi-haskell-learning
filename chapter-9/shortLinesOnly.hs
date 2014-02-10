main :: IO ()
--        String  <- IO String
main = do content <- getContents
      -- ((String -> IO ())
      --    ((String -> String) String))
          putStr (shortLinesOnly content)

shortLinesOnly :: String -> String
--               unlines . (filter (isShort 10)) . lines
shortLinesOnly = unlines . (filter $ isShort 10) . lines

isShort :: Int -> String -> Bool
isShort n line = length line < n
