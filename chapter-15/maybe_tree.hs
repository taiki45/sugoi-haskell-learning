module Tree where

import Control.Monad
import Data.Char

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving Show

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty))
                (Node 'Y'
                    (Node 'S' Empty Empty)
                    (Node 'A' Empty Empty)))
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty))
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)))

data Direction = L
               | R
               deriving Show

type Directions = [Direction]

data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
             deriving Show

type Breadcrumbs a = [Crumb a]
type Error a = Either String a

goLeft :: Zipper a -> Error (Zipper a)
goLeft (Node a l r, bs) = return (l, LeftCrumb a r:bs)
goLeft (Empty,_) = Left "miss in left"

goRight :: Zipper a -> Error (Zipper a)
goRight (Node a l r, bs) = return (r, RightCrumb a l:bs)
goRight (Empty,_) = Left "miss in right"

goUp :: Zipper a -> Error (Zipper a)
goUp (l, LeftCrumb a r:bs) = return (Node a l r, bs)
goUp (r, RightCrumb a l:bs) = return (Node a l r, bs)
goUp (_, []) = Left "up miss"


(-:) :: a -> (a -> b) -> b
a -: f = f a

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Error (Zipper a)
topMost z@(_, []) = return z
topMost z = goUp z >>= topMost
