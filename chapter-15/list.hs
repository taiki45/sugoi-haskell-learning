module List where

type Crumb a = [a]
type Zipper a = ([a], Crumb a)

goDown :: Zipper a -> Zipper a
goDown (x:xs, bs) = (xs, x:bs)
goDown ([], _) = error "Empty List"

goUp :: Zipper a -> Zipper a
goUp (xs, b:bs) = (b:xs, bs)
goUp (_, []) = error "Empty Crumb"

topMost :: Zipper a -> Zipper a
topMost z@(_, []) = z
topMost z = topMost $ goUp z

(-:) :: a -> (a -> b) -> b
a -: f = f a

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (x:xs, bs) = (f x:xs, bs)
modify _ ([], _) = error "Empty List"

attach :: [a] -> Zipper a -> Zipper a
attach l (_, bs) = (l, bs)
