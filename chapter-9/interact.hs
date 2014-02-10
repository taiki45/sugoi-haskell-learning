main :: IO ()
main = interact $ shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . (filter $ isShort 10) . lines

isShort :: Int -> String -> Bool
isShort n line = length line < n
