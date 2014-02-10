{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do txt <- T.getLine
          T.putStrLn $ T.append "ε( ε^o^)э " txt
