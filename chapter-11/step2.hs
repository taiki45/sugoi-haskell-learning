module ParserCombinator where

newtype Parser s a = P ([s] -> [(a,[s])])

parse :: Parser s a -> [s] -> [(a,[s])]
parse (P p) s = p s

satisfy :: (s -> Bool) -> Parser s s
satisfy p = P f
    where f (x:xs) | p x       = [(x,xs)]
                   | otherwise = []

char :: Eq s => s -> Parser s s
char c = satisfy (c ==)
