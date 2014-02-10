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




-- ===============================
shortLinesOnly' :: String -> String
-- content is like:
--      abdfg
--      1234567890123
--      AAAAABBB
shortLinesOnly' content = let contentLines = lines content
                                            -- filter :: (a -> Bool) -> [a] -> [a]
                              onlyShortLines = filter (\line -> length line < 10) contentLines
                           in unlines onlyShortLines
