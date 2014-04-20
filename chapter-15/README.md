Zipper

データ構造の更新を簡単に、そして Traversing を効率的にしてくれる Zipper.

```haskell
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
```

`elemAt` と `changeP` を試す。

```haskell
ghci> elemAt [R, L] freeTree
'W'

ghci> elemAt [R,L] $ changeToP [R, L] freeTree
'P'
```

OK.

### 書き換えを効率よくしたい！
部分木に注目する良い方法。まずはたどった道を残す。

```haskell
ghci> goLeft (goRight (freeTree, []))
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
```

```haskell
type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)
goLeft (Empty,_) = error "Empty node"

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)
goRight (Empty,_) = error "Empty node"
```

もっと見栄えを良くしよう。

```haskell
(-:) :: a -> (a -> b) -> b
a -: f = f a
```

```haskell
ghci> (freeTree, []) -: goRight -: goLeft
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
```

OK.

### 上に辿りたい！

今わかっていること

- 今の木が親の木の右/左部分木なこと
- その親の親の木の...

上に辿りたいのであれば...

- 親の木のルート要素
- 親の木の部分木

がそれぞれ必要。

```haskell
data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
             deriving Show

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node a l r, bs) = (l, LeftCrumb a r:bs)
goLeft (Empty,_) = error "Empty node"

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node a l r, bs) = (r, RightCrumb a l:bs)
goRight (Empty,_) = error "Empty node"
```

そして goUp

```haskell
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (l, LeftCrumb a r:bs) = (Node a l r, bs)
goUp (r, RightCrumb a l:bs) = (Node a l r, bs)
goUp (_, []) = error "Empty Breadcrumbs"
```

試す

```haskell
ghci> (freeTree, []) -: goLeft -: goUp
(Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))),[])
```

元に戻った！！！

### Zipper
あるデータ構造の一部分の注目点とその周辺データを保持しているデータ構造を Zipper と一般に呼ぶ。

今見ている要素を書き換える関数

```haskell
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)
```

左の部分木のルート要素を書き換えて元に戻る。

```haskell
import Data.Char

ghci> (freeTree, []) -: goLeft -: modify toLower -: goUp
(Node 'P' (Node 'o' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))),[])
```

さらに右も書き換える例

```haskell
ghci> (freeTree, []) -: goLeft -: modify toLower -: goUp -: goRight -: modify toLower -: goUp
(Node 'P' (Node 'o' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'l' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))),[])
```

今見ている部分木を書き換える

```haskell
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

ghci> (freeTree, []) -: goLeft -: modify toLower -: goUp -: goRight -: attach (Node 'X' Empty Empty) -: goUp
(Node 'P' (Node 'o' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'X' Empty Empty),[])
```

一番上に戻る操作がほしい。

```haskell
topMost :: Zipper a -> Zipper a
topMost z@(_, []) = z
topMost z = topMost $ goUp z

ghci> (freeTree, []) -: goLeft -: modify toLower -: goRight -: goLeft -: topMost
(Node 'P' (Node 'o' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))),[])
```

## List でも Zipper
```hasklell
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
```

Let's play!

```haskell
ghci> ([1..10], []) -: goDown -: goDown -: modify (*10) -: topMost
([1,2,30,4,5,6,7,8,9,10],[])

ghci> ([1..10], []) -: goDown -: goDown -: attach [0] -: topMost
([1,2,0],[])
```