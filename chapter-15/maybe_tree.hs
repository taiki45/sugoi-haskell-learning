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

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r
changeToP _ Empty = error "Empty node"

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node a _ _) = a
elemAt _ Empty = error "Empty node"

data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
             deriving Show

type Breadcrumbs a = [Crumb a]

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node a l r, bs) = return (l, LeftCrumb a r:bs)
goLeft (Empty,_) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node a l r, bs) = return (r, RightCrumb a l:bs)
goRight (Empty,_) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (l, LeftCrumb a r:bs) = return (Node a l r, bs)
goUp (r, RightCrumb a l:bs) = return (Node a l r, bs)
goUp (_, []) = Nothing


(-:) :: a -> (a -> b) -> b
a -: f = f a

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Maybe (Zipper a)
topMost z@(_, []) = return z
topMost z = goUp z >>= topMost
