module ParserCombinator where

import Control.Applicative
import Data.Char

newtype Parser s a = P { parse :: [s] -> [(a,[s])] }

instance Functor (Parser s) where
        fmap f (P g) = P (\s -> [(f r1,s1) | (r1,s1) <- g s])

instance Applicative (Parser s) where
        pure = success
        (P p1) <*> (P p2) = P (\s ->
                                [(f a,xs) | (f,_) <- p1 s, (a,xs) <- p2 s])

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
    | otherwise = let r@((_,rest):_) = result
                      as = map fst r
                    in rec p rest (acc ++ as)
    where result = p str

success :: a -> Parser s a
success a = P (\s -> [(a,s)])

failure :: Parser s a
failure = P (\_ -> [])

satisfy :: (s -> Bool) -> Parser s s
satisfy p = P f
    where f (x:xs) | p x       = [(x,xs)]
                   | otherwise = []

char :: Eq s => s -> Parser s s
char c = satisfy (c ==)

natural :: Parser Char Integer
natural = read <$> some (satisfy isDigit)
