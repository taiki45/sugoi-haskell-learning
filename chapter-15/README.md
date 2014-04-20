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

