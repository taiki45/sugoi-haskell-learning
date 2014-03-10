{-
- $ runghc main.hs '(+ (- 5 4) 5)'
- Right (Number 6)
-}

import Control.Applicative
import System.Environment (getArgs)

import ParserCombinator

data Value = Number Integer
           | String String
           | Atom String
           | List [Value]
           deriving Show

type Func = [Value] -> Value
type Env = [(String, Func)]
type RunTimeError = Either String

main :: IO ()
main = do args <- getArgs
          case args of
              (expr:_) -> run expr
              _        -> putStrLn "No args"

run :: String -> IO ()
run input = case parse parseExpr input of
                ((r, _):_) -> putStrLn . show $ eval defaultEnv r
                _ -> putStrLn "invalid input"

eval :: Env -> Value -> RunTimeError Value
eval _ (Number a) = return (Number a)
eval _ (String a) = return (String a)
--eval env (Atom a) = lookup a env
eval env (List ((Atom a):xs)) = do f <- lookupEnv a env
                                   args <- sequence (map (eval env) xs)
                                   apply f args

-- TODO: add type error handling
apply :: Func -> [Value] -> RunTimeError Value
apply f args = return $ f args

lookupEnv :: String -> Env -> RunTimeError Func
lookupEnv key env = case lookup key env of
                      Just a -> return a
                      Nothing -> Left $ "Can't find: " ++ key

defaultEnv :: Env
defaultEnv = [("+", add)
             ,("-", sub)]
             where add = foldr (numBinOp (+)) (Number 0)
                   sub = foldr1 (numBinOp (-))

numBinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
numBinOp op (Number x) (Number y) = Number $ op x y
numBinOp _ _ _ = error "Type Error! : numBinOp"


parseAtom :: Parser Char Value
parseAtom = Atom <$> some (alphabet <|> oneOf "!%^&*-+_?<>")

parseNumber :: Parser Char Value
parseNumber = Number <$> natural

parseString :: Parser Char Value
parseString = String <$> (char '"'
                          *> many (noneOf "\"")
                          <* char '"')

parseList :: Parser Char Value
parseList = List <$> listOf parseExpr spaces

parseExpr :: Parser Char Value
parseExpr = parseAtom
            <|> parseNumber
            <|> parseString
            <|> (char '(' *> parseList <* char ')')
