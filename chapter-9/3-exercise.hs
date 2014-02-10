import Control.Exception
import System.Environment
import System.Random

main :: IO ()
main = do arg <- getArgs
          case arg of
              ("lock":_) -> lock
              ("open":_) -> openSecret
              _          -> showHelp

passwordPath :: String
passwordPath = "password.txt"

randomString :: IO String
randomString = do gen <- getStdGen
                  return $ take 10 $ randomRs ('0', 'Z') gen

lock :: IO ()
lock = do password <- randomString
          writeFile passwordPath password

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
