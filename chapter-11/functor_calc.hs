main :: IO ()
main = do name <- getLine
          let result = lookupNumber name
              adress = fmap numberToAdress result
          case adress of
              (Just s) -> putStrLn $ "Adress is " ++ s
              Nothing  -> putStrLn "Failed"

numberList :: [(String, String)]
numberList = [("Bob", "090")
           ,("John", "080")
           ,("Taiki", "070")]

lookupNumber :: String -> Maybe String
lookupNumber name = lookup name numberList

numberToAdress :: String -> String
numberToAdress "090" = "Tokyo"
numberToAdress "080" = "Akashi"
numberToAdress "070" = "Kyoto"
