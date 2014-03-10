module ParserCombinator where

import Data.Char

type Parser s a = [s] -> [(a,[s])]

satisfy :: (s -> Bool) -> Parser s s
satisfy p (x:xs) | p x = [(x,xs)]
satisfy _ _            = []

char :: Eq s => s -> Parser s s
char c = satisfy (==c)

token :: Eq s => [s] -> Parser s [s]
token t ls | t == take n ls = [(t,drop n ls)]
           | otherwise      = []
             where n = length t

success :: a -> Parser s a
success v xs = [(v,xs)]

failure :: Parser s a
failure _ = []

infixr 6 <&>
(<&>) :: Parser s a -> Parser s b -> Parser s (a,b)
p <&> q = \ls -> [((r1,r2),s2) | (r1,s1) <- p ls, (r2,s2) <- q s1]

infixr 4 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = \ls -> p ls ++ q ls

infixl 5 <@
(<@) :: Parser s a -> (a -> b) -> Parser s b
p <@ f = \ls -> [(f r,s) | (r,s) <- p ls]

infixr 6 <&
(<&) :: Parser s a -> Parser s b -> Parser s a
p <& q = p <&> q <@ fst

infixr 6 &>
(&>) :: Parser s a -> Parser s b -> Parser s b
p &> q = p <&> q <@ snd

many :: Parser s a -> Parser s [a]
many p = p <&> many p <@ (\(x,xs) -> x:xs)
     <|> success []

many1 :: Parser s a -> Parser s [a]
many1 p = p <&> many p <@ \(x,xs) -> x:xs

natural :: Parser Char Int
natural = many1 (satisfy isDigit) <@ read

first :: Parser s a -> Parser s a
first p = take 1 . p

manyf :: Parser s a -> Parser s [a]
manyf  p = first (many  p)
many1f :: Parser s a -> Parser s [a]
many1f p = first (many1 p)

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = p <&> many (s &> p) <@ (\(l,ls) -> l:ls)
         <|> success []

commaSep :: Parser Char a -> Parser Char [a]
commaSep p = listOf p (char ',')
