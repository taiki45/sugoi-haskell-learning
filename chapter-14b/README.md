# 14章 もうちょっとだけモナド
memo: Control.Monad を Control.Applicative を import しておきましょう。

## 14.4 Error を壁に
Maybe も Either も失敗を表せる。Maybe は失敗した計算の以降の計算を全てスキップすることによって、失敗を表現している。Either は同じように全てスキップするが、値と共にスキップできる。

Right で成功 + 値を、Left で失敗 + 値を持つ。

Functor として考えるとわかりやすい。Left value は mapping されない、Right value は mapping される。なので、Left の場合は一切の計算が mapping されず、 Right の場合は以降の計算が mapping される。このようにしてエラー処理を実現している。

```haskell
fmap (+4) Left "abc"
fmap (+4) Right 5
```

### 現在の実装
```haskell
data  Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show, Typeable)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Monad (Either e) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r
```

Typeable http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Typeable.html

### コラム: Either のもうひとつの側面
エラー処理に使われる以外にもっと一般的に使える。

文脈付きの or。片方のみに処理をする、文脈によって処理をわける、ということが可能。https://gist.github.com/taiki45/8007954

## 14.5 モナディック関数特集
### liftM, ap
leftM は fmap と同じ。ap は <*> と同じ。

```haskell
reverse `liftM` getLine
reverse `fmap` getLine

((*) <$> [1..3]) `ap` [5..7]
(*) <$> [1..3] <*> [5..7]
```

### join
モナドの力の源。Applicative Functor と Monad をわける境界。自身の重なりを平らにする。

```haskell
ghci> :t Just $ Just 5
Just $ Just 5 :: Num a => Maybe (Maybe a)

ghci> :t join $ Just (Just 5)
join $ Just (Just 5) :: Num a => Maybe a
```

みんな大好き従業員リストと企業名の検索を考えてみましょう。employees には (従業員名,企業ID) が入っています。companies には (企業ID,企業名) が入っています。やることとしては、まず従業員名から企業IDを引き出して、その結果を使ってさらに企業名を探し出します。この従業員リストと企業リストは不完全なので対象が見つからないことがあります。

```haskell
ghci> let employees = [("taiki", 123), ("bob", 256), ("john", 16)
ghci> let companies = [(123,"OsakanaSuisan"), (256,"TheSUSHI")]

ghci> lookup "taiki" employees >>= \name -> lookup name companies
Just "OsakanaSuisan"

ghci> lookup "john" employees >>= \name -> lookup name companies
Nothing
```

さてここで join の重要な働きをみてみます。そのためにまずモナドの力を使わずにやりたいことをやってみます。

```haskell
ghci> :t lookup "taiki" employees
Num b => Maybe b -- これが企業ID検索の結果の型

ghci> :t flip lookup companies
(Num a, Eq a) => a -> Maybe [Char] -- 企業ID -> 企業名の検索結果

-- なんか fmap で検索はとりあえずできそう

ghci> flip lookup companies `fmap` lookup "taiki" employees
Just (Just "OsakanaSuisan")

ghci> flip lookup companies `fmap` lookup "john" employees
Just Nothing
```

fmap を使えば一応検索はできました。でも検索結果の型がとても扱いづらいです。Maybe なら多大な努力をしてパターンマッチでとりだせますが、IO ならどうしましょう？

```haskell
ghci> :t putStrLn . reverse <$> getLine
putStrLn . reverse <$> getLine :: IO (IO ())
```

これを私達は Haskell の世界からどうにかできそうには思えません！これが Functor や Applicative Functor の力の限界です。

このように、文脈付きの値の使ってさらに文脈付きの値を返すような処理はよくあります。そこで、モナドというアイディアが登場がするのです！

誤解を恐れずにいうとモナドというパターンは、join を持った Functor と考えましょう、ということです。join は Functor の重なりを平らにする関数です。これがあれば bind (>>=) がなくても上のような問題は解決できます！

```haskel
ghci> join $ flip lookup companies `fmap` lookup "taiki" employees
Just "OsakanaSuisan"

ghci> join $ flip lookup companies `fmap` lookup "john" employees
Nothing
-- ちゃんと文脈を保ったまま平らになる！

ghci> :t join $ putStrLn . reverse <$> getLine
join $ putStrLn . reverse <$> getLine :: IO ()
```

本にもあるように bind(>>=) は join を使って定義できます！また逆に join も bind(>>=) をつかって定義できるんです。

```haskell
ghci> let m `bind` f = join $ f `fmap` m
ghci> let join_ mm = mm >>= id
```

実際にプログラミングを行うには >>= のような演算子のほうが格段に便利なので Haskell ではモナドは pure と >>= を定義するようになっています。でも実は join を考えたほうがモナドの力の源がわかりやすかったりします :)

### filterM
```haskell
ghci> let powerset xs = filterM (\_ -> [True, False]) xs
ghci> powerset [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
```

```haskell
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do result <- p x
                       if result
                           then do xs' <- filterM' p xs
                                   return $ x:xs'
                           else filterM' p xs
```

filterM を実装してみました。実装をもとに処理をおっかけていけば powerset の不思議な力もわかります。

### foldM, foldM_
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
```

```haskell
ghci> foldM_ (\a b -> print (a+b) >> return (a*b)) 1 [1..5]
2
3
5
10
29
```

## 14.7 モナディック関数の合成
```haskell
ghci> let lookup_ = flip lookup
ghci> :t lookup_ employees >=> lookup_ companies
lookup_ employees >=> lookup_ companies :: [Char] -> Maybe [Char]

ghci> lookup_ employees >=> lookup_ companies $ "taiki"
Just "OsakanaSuisan"
ghci> lookup_ employees >=> lookup_ companies $ "john"
Nothing
```

## 14.6 安全な逆ポーランド記法電卓
### 通常 ver.
```haskell
module RpnSolver where

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

-- スタックと演算子の都合があえば計算
-- そうでなければ数字をスタックに積む
foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:ys) "*" = (y * x):ys
foldingFunction (x:y:ys) "+" = (y + x):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction xs n = read n:xs
```

```haskell
ghci> solveRPN "4 5 3 +"
8.0
ghci> solveRPN "4 5 3 a"
*** Exception: Prelude.read: no parse
ghci> solveRPN "4 5 3 ++"
*** Exception: Prelude.read: no parse
```

### Maybe 版
```haskell
module RpnSolver where

import Control.Monad

solveRPN :: String -> Maybe Double
solveRPN = fmap head . foldM foldingFunction [] . words

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return $ (y * x):ys
foldingFunction (x:y:ys) "+" = return $ (y + x):ys
foldingFunction (x:y:ys) "-" = return $ (y - x):ys
foldingFunction xs s = case reads s of
                           (n,_):_ -> return $ n:xs
                           _      -> Nothing
```

```haskell
ghci> solveRPN "4 5 +"
Just 9.0
ghci> solveRPN "4 5 3 +"
Just 8.0
ghci> solveRPN "4 5 3 a"
Nothing
ghci> solveRPN "4 5 3 ++"
Nothing
```

### Either 版

```haskell
module RpnSolver where

import Control.Monad

type Error = Either String

solveRPN :: String -> Error Double
solveRPN = fmap head . foldM foldingFunction [] . words

foldingFunction :: [Double] -> String -> Error [Double]
foldingFunction (x:y:ys) "*" = return $ (y * x):ys
foldingFunction (x:y:ys) "+" = return $ (y + x):ys
foldingFunction (x:y:ys) "-" = return $ (y - x):ys
foldingFunction xs s = case reads s of
                           (n,_):_ -> return $ n:xs
                           _      -> Left $ "can't match with given string: " ++ s
```

```haskell
ghci> solveRPN "4 5 3 +"
Right 8.0
ghci> solveRPN "4 5 3 ++"
Left "can't match with given string: ++"
ghci> solveRPN "4 5 3 a"
Left "can't match with given string: a"
```

## 14.8 モナドを作る
```haskell
module CreateMonad where

import Control.Applicative
import Data.Ratio
import Data.Char

newtype Prob a = Prob { getProb :: [(a,Rational)] }
               deriving Show

-- ghci> fmap negate $ Prob [(2,1%2),(3,1%4),(4,1%4)]
-- Prob {getProb = [(-2,1 % 2),(-3,1 % 4),(-4,1 % 4)]}
instance Functor Prob where
         f `fmap` (Prob xs) = Prob $ fstmap f <$> xs
            where fstmap :: (a -> c) -> (a,b) -> (c,b)
                  g `fstmap` (a,b) = (g a,b)

instance Applicative Prob where
        pure = return
        (Prob fs) <*> (Prob xs) = Prob $ mul `map` fs <*> xs
            where mul (f,x) (a,y) = (f a,x*y)

instance Monad Prob where
        return a = Prob [(a,1)]
        m >>= f = flatten $ f <$> m

-- Prob
--   [ (Prob [('a',1%2), ('b',1%2)],1%4)
--   , (Prob [('c',1%2), ('d',1%2)[,3%4) ]
--
-- unwrap (Prob [('a',1%2), ('b',1%2)], 1%4)
--   -> [('a',1%8), ('b',1%8)]
--
-- app 1%4 ('a',1%2)
--   -> ('a',1%8)
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob . concat $ unwrap `map` xs
    where unwrap :: (Prob a,Rational) -> [(a,Rational)]
          unwrap (Prob ls,x) = map (app x) ls
          app :: Rational -> (a,Rational) -> (a,Rational)
          app x = fmap (x *)

testA :: Prob Char
testA = Prob [('a',1%2), ('b',1%2)]

testB :: Prob Char
testB = Prob [('c',1%2), ('d',1%2)]

testF :: Prob (Char -> Int)
testF = Prob [(ord,1%4), (ord,3%4)]

testCartesian :: Prob (Prob Char)
testCartesian = Prob [(testA,1%4), (testB,3%4)]
```

試してみましょう。

```haskell
ghci> ord <$> testA
Prob {getProb = [(97,1 % 2),(98,1 % 2)]}

ghci> testF <*> testA
Prob {getProb = [(97,1 % 8),(98,1 % 8),(97,3 % 8),(98,3 % 8)]}

ghci> flatten testCartesian
Prob {getProb = [('a',1 % 8),('b',1 % 8),('c',3 % 8),('d',3 % 8)]}

ghci> testA >>= (\a -> return $ ord a)
Prob {getProb = [(97,1 % 2),(98,1 % 2)]}

ghci> testA >>= \c -> Prob [([c,'a'], 3%4), ([c,'b'], 1%4)]
Prob {getProb = [("aa",3 % 8),("ab",1 % 8),("ba",3 % 8),("bb",1 % 8)]}
```

## 演習. リストモナドをつくる
- ステップ1. join のような関数 `join_` を定義して、モナドを平らにすることを考えましょう。モナドのインスタンス宣言では m >>= f = join_ $ f `fmap` m で済ませてしましょうましょう。
- ステップ2. join_ のことは一旦忘れて力ずくで >>= を実装してみましょう。

参考回答 https://gist.github.com/taiki45/8334528