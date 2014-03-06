module ParserCombinator where

import Control.Applicative
import Data.Char

-- for S-exp
data Value = Atom String
           | Number Integer
           | String String
           | List [Value]
           deriving Show

newtype Parser s a = P { parse :: [s] -> [(a,[s])] }

instance Functor (Parser s) where
        fmap f (P g) = P (\s -> [(f r1,s1) | (r1,s1) <- g s])

instance Applicative (Parser s) where
        pure = success
        (P p1) <*> (P p2) = P (\s ->
                                [(f a,s2) | (f,s1) <- p1 s, (a,s2) <- p2 s1])

instance Alternative (Parser s) where
        empty = failure
        (P p1) <|> (P p2) = P (\s ->
                                (p1 s) ++ (p2 s))
        many (P p) = P (\s -> rec p s [])
        some (P p) = P (\s -> if valid s then rec p s [] else [])
                where valid s = not $ null (p s)

rec :: ([s] -> [(a,[s])]) -> [s] -> [a] -> [([a],[s])]
rec p str acc
    | null $ result = [(acc, str)]
    | otherwise = let ((_,rest):_) = result
                      as = map fst result
                    in rec p rest (acc ++ as)
    where result = p str

infixr 6 <&>
(<&>) :: Parser s a -> Parser s b -> Parser s (a,b)
(P p) <&> (P q) = P (\ls -> [((r1,r2),s2) | (r1,s1) <- p ls, (r2,s2) <- q s1])

success :: a -> Parser s a
success a = P (\s -> [(a,s)])

failure :: Parser s a
failure = P (\_ -> [])

satisfy :: (s -> Bool) -> Parser s s
satisfy p = P f
    where f (x:xs) | p x       = [(x,xs)]
                   | otherwise = []
          f _ = []

char :: Eq s => s -> Parser s s
char c = satisfy (c ==)

natural :: Parser Char Integer
natural = read <$> some (satisfy isDigit)

oneOf :: Eq s => [s] -> Parser s s
oneOf s = satisfy (flip elem s)

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = (\(l,ls) -> l:ls) <$> p <&> many (s *> p)
         <|> pure []

alphabet :: Parser Char Char
alphabet = oneOf $ ['A'..'Z'] ++ ['a'..'z']

spaces :: Parser Char String
spaces = many $ oneOf " \n\r"

noneOf :: Eq s => [s] -> Parser s s
noneOf s = satisfy (not . flip elem s)

parseNumber :: Parser Char Value
parseNumber = Number <$> natural

parseString :: Parser Char Value
parseString = String <$> (char '"'
                          *> many (noneOf "\"")
                          <* char '"')

parseAtom :: Parser Char Value
parseAtom = Atom <$> (some $ oneOf "+-*/%" <|> alphabet)

parseList :: Parser Char Value
parseList = List <$> listOf parseExpr spaces

parseExpr :: Parser Char Value
parseExpr = parseAtom
            <|> parseNumber
            <|> parseString
            <|> (char '('
                 *> parseList
                 <* char ')')
