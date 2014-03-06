module ParserCombinator where

import Control.Applicative

newtype Parser s a = P { parse :: [s] -> [(a,[s])] }

instance Functor (Parser s) where
        fmap f (P g) = P (\s -> [(f r1,s1) | (r1,s1) <- g s])

instance Applicative (Parser s) where
        pure = success
        (P p1) <*> (P p2) = P (\s ->
                                [(f a,xs) | (f,_) <- p1 s, (a,xs) <- p2 s])

success :: a -> Parser s a
success a = P (\s -> [(a,s)])

satisfy :: (s -> Bool) -> Parser s s
satisfy p = P f
    where f (x:xs) | p x       = [(x,xs)]
                   | otherwise = []

char :: Eq s => s -> Parser s s
char c = satisfy (c ==)
