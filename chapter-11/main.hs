{-
- $ runghc main.hs '(+ (- 5 4) 5)'
- Number 6
-}

import Control.Applicative
import System.Environment

import ParserCombinator

data Value = Number Integer
           | String String
           | Atom String
           | List [Value]
           deriving Show

type Func = [Value] -> Value
type Env = [(String, Func)]

main :: IO ()
main = do args <- getArgs
          case args of
              (expr:_) -> run expr
              _        -> putStrLn "No args"

run :: String -> IO ()
run input = case parse parseExpr input of
                ((r, _):_) -> case eval defaultEnv r of
                                  Just a -> putStrLn . show $ a
                                  Nothing -> putStrLn "fail"
                _ -> putStrLn "invalid input"

eval :: Env -> Value -> Maybe Value
eval _ (Number a) = Just (Number a)
eval _ (String a) = Just (String a)
--eval env (Atom a) = lookup a env
eval env (List ((Atom a):xs)) = do f <- lookup a env
                                   apply f <$> sequence (map (eval env) xs)

-- TODO: Func -> [Value] -> Either String Value
apply :: Func -> [Value] -> Value
apply f args = f args

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
