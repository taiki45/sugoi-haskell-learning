module ParserCombinator
    ( Parser
    , parse
    , satisfy
    , char
    , digit
    , natural
    , token
    , oneOf
    , listOf
    , alphabet
    , spaces
    , noneOf )
where

-- ref: http://d.hatena.ne.jp/tanakh/20040731

import Control.Applicative
import Data.Char

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
        -- TODO: duplicated function calling ^

rec :: ([s] -> [(a,[s])]) -> [s] -> [a] -> [([a],[s])]
rec p str acc
    | null $ result = [(acc, str)]
    | otherwise = let r@((_,rest):_) = result
                      as = map fst r
                    in rec p rest (acc ++ as)
    where result = p str

instance Monad (Parser s) where
        return = success
        (P p1) >>= f = P (\s -> do (r1,s1) <- p1 s
                                   let (P p2) = f r1
                                   (r2,s2) <- p2 s1
                                   return (r2,s2))


success :: a -> Parser s a
success a = P (\s -> [(a,s)])

failure :: Parser s a
failure = P (\_ -> [])

satisfy :: (s -> Bool) -> Parser s s
satisfy p = P f
    where f (x:xs) | p x = [(x,xs)]
                   | otherwise = []
          f _ = []

char :: Char -> Parser Char Char
char c = satisfy (c ==)

digit :: Parser Char Char
digit = satisfy isDigit

natural :: Parser Char Integer
natural = read <$> some digit

oneOf :: Eq s => [s] -> Parser s s
oneOf s = satisfy (flip elem s)

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = (do a <- p
                 as <- many (s *> p)
                 return (a:as))
             <|> pure []

alphabet :: Parser Char Char
alphabet = oneOf $ ['A'..'Z'] ++ ['a'..'z']

spaces :: Parser Char String
spaces = many $ oneOf " \n\r"

noneOf :: Eq s => [s] -> Parser s s
noneOf s = satisfy (not . flip elem s)

token :: Eq s => [s] -> Parser s [s]
token t = P f
    where f ts | t == take n ts = [(t,drop n ts)]
               | otherwise = []
          n = length t
