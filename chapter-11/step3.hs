module ParserCombinator where

newtype Parser s a = P { parse :: [s] -> [(a,[s])] }

instance Functor (Parser s) where
        fmap f (P g) = P (\s -> [(f r1,s1) | (r1,s1) <- g s])

satisfy :: (s -> Bool) -> Parser s s
satisfy p = P f
    where f (x:xs) | p x       = [(x,xs)]
                   | otherwise = []
          f _ = []

char :: Eq s => s -> Parser s s
char c = satisfy (c ==)
