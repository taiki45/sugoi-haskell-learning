import Control.Exception
import System.Environment

main :: IO ()
main = do arg <- getArgs
          case arg of
              ("lock":password:_) -> lock password
              ("open":_)          -> openSecret
              _                   -> showHelp

passwordPath :: String
passwordPath = "password.txt"

lock :: String -> IO ()
lock = writeFile passwordPath

openPassword :: IO String
openPassword = readFile passwordPath

openSecret :: IO ()
openSecret = do password <- openPassword
                result   <- tryMatching password
                while not result $ do
                    putStrLn "Wrong!!"
                    tryMatching password
                putStrLn "Got it!!\n"
                putStrLn $ unwords $ take 13 $ repeat "* "
                putStrLn "you know L, reapers eat only apples..."

-- True is success
tryMatching :: String -> IO Bool
tryMatching password = do putStrLn "What is password?"
                          putStr "> "
                          input <- getLine
                          return $ password == input

while :: (a -> Bool) -> a -> IO a -> IO a
while pred' a body
    | pred' a   = do result <- body
                     while pred' result body
    | otherwise = return a

showHelp :: IO ()
showHelp = do putStrLn "=Usage="
              putStrLn "lock:"
              putStrLn "    passwordProg.hs lock 'your password'"
              putStrLn "open:"
              putStrLn "    passwordProg.hs open"
