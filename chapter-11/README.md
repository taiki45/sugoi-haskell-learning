# 11章

* 全てのモナドはアプリカティブファンクターである
* 全てのアプリカティブファンクターはファンクターである

実装上は歴史的経緯と考慮すべき事情により上述のようになっていないが、全てのモナドはファンクターである。実装上もほとんどのモナドはファンクターなのでこれから紹介する便利な機能や性質が使える。ref: http://itpro.nikkeibp.co.jp/article/COLUMN/20120110/378061/?ST=develop&P=5

## Functor

* ファンクターは文脈付きの値、あるいは**計算**。
* 「X がファンクターである」と言えることは「X は函数で写せるなにがである」ということ。
* `fmap` は函数の持ち上げと適用を行う。
* 「ファンクタ = 箱」と考えていると関数の性質を持つファンクタで死ぬので計算とｲﾒｯｼﾞしておくのが無難 ※個人の意見

```
instance Functor Maybe -- Defined in `Data.Maybe'
instance Functor (Either a) -- Defined in `Data.Either'
instance Functor [] -- Defined in `GHC.Base'
instance Functor IO -- Defined in `GHC.Base'
instance Functor ((->) r) -- Defined in `GHC.Base'
instance Functor ((,) a) -- Defined in `GHC.Base'
```

## ファンクター則
### 1
ファンクターF, 型 A について (F idA) = (F A)id

```haskell
fmap (id :: a -> a) = (id :: Functor f => f a -> f a)
```

((型Aのid函数)を持ち上げたもの) = ((持ち上げた型A)のid函数)

※ `id` 函数は多相的である

```haskell
fmap (id :: Int -> Int) $ Just 4 = (id :: Functor f => f Int -> f Int) $ Just 4
```

### 2
ファンクターF, 型a, b, c, 函数 f: a -> b, g: b -> c について、(F g) ○ (F f) = F (g ○ f)

```haskell
f x = x + 1
g x = x * 5
(fmap g) . (fmap f) $ Just 5 = Just $ (g . f) 5
```

### Data.Functor
基本となる functor functions 以外に便利 functions が定義されてる。

後で学ぶモナドと組み合わせる時に、`<$` 演算子は複数の計算から一つの大きな計算を組み立てる際、組み立てる計算の中に何らかの作用を持つ計算を組み込むのに役立ちます。

```haskell
-- | An infix synonym for 'fmap'.
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const

-- Usage
"Replaced" <$ Just 5 -- Just "Replaces"
```

### Free Monad
すべてのファンクターから Free Monad というモナドが導出できます

* http://hackage.haskell.org/package/free
* http://d.hatena.ne.jp/fumiexcel/20121111/1352614885
* http://myuon-myon.hatenablog.com/entry/20121111/1352608035

## Pointed Functor
* 最小のファンクター値を作る函数`point`を備えているファンクター
* Category の概念を使ってきれいに抽象化するようなライブラリで出現する i.e. Control.Lens
* `point` は Applicative Functor の `pure` と同じ。

## Applicative Functor
* 文脈付きの値または**計算の結果や過程**に対して Functor よりも様々な操作ができる強化版 Functor。
* 最小のファンクター値を作る函数 `pure` を備えている。
* 包まれた or 持ち上げられた函数にファンクター値で適用できる函数`<*>`を備えている。
* monadic functions ではなく、Applicative functions を使って書くことを Applicative Stryle と呼ぶらしい。Applicative Style が効果的なのはパーサコンビネータを使う時。
* ちょっと便利なファンクター、という感覚。
* アプリカティブのススメ http://d.hatena.ne.jp/kazu-yamamoto/20101211/1292021817
* paper をご所望ですか？ http://www.soi.city.ac.uk/~ross/papers/Applicative.html

```haskell
pure (+) <*> Just 4 <*> Just 3
-- Just 7
pure (+) <*> Nothing <*> Just 3
-- Nothing

-- lookup :: a -> [(a,b)] -> Maybe b
pure (+) <*> lookup "Bob" nameAgeList <*> 10
```

```
instance Applicative [] -- Defined in `Control.Applicative'
instance Applicative ZipList -- Defined in `Control.Applicative'
instance Monad m => Applicative (WrappedMonad m)
  -- Defined in `Control.Applicative'
instance Applicative Maybe -- Defined in `Control.Applicative'
instance Applicative IO -- Defined in `Control.Applicative'
instance Applicative (Either e) -- Defined in `Control.Applicative'
instance Applicative ((->) a) -- Defined in `Control.Applicative'
```

### Applicative functions

```haskell
class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

    -- | Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    (*>) = liftA2 (const id)

    -- | Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const
```

Usage

```haskell
Just 5 *> pure 0 -- Just 0
pure "fail" <* getLine -- IO "fail"
```

### アプリカティブ則
ファンクタF, 型A,B,C,Ds, 関数f:A->B,g:B->C

Fで持ち上げられた型Aを`FA`と書く、Fで持ち上げられた関数fを`Ff`と書く、Aのidを`Aid`と書く、FAのidを`FAid`と書く

#### identity 単位元律
`pure id <*> v = v`, この時 `v :: (F A)`, `id :: A -> A`

(F(Aid))(FA) = FA、idにフォーカスすると F(Aid) = FAid

「持ち上げたidで持ち上げた型を写す = 持ち上げた型」、idにフォーカスすると「持ち上げたid = 持ち上げた型のid」※ Haskellのidは多相的なので表現を変えている

#### composition 合成
`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

(Fg○Ff)(FA) = Fg(Ff(FA)) = FC

合成の性質を保存する

#### homomorphism 準同型
`pure f <*> pure x = pure (f x)`

Ff(FA) = F(f(A))

持ち上げた型を持ち上げた関数で写す = 関数で写した結果を持ち上げる

#### interchange 
`u <*> pure y = pure ($ y) <*> u`

`$` 演算子を使うことで演算の順序を入れ替えても等価になること。

http://planetmath.org/interchangelaw

#### Maybe でやってみる
```haskell
-- 1. identity
ghci> (pure id <*> Just 0) == Just 0
True
-- 2.composition
ghci>  pure (.) <*> pure (+1) <*> pure (*5) <*> Just 3
Just 16
ghci> pure (+1) <*> (pure (*5) <*> Just 3)
Just 16
-- 3. homomorphism
ghci> (pure succ <*> Just 3) == (pure $ succ 3)
True
-- 4.interchange
ghci> (Just succ <*> pure 3) == (pure ($ 3) <*> Just succ)
True
```

### ZipList
`zipWithN` の抽象。

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

-- 以下は等価
zipWith (+) [1..10] [21..30] -- [22,24,26,28,30,32,34,36,38,40]
getZipList $ (+) <$> ZipList [1..10] <*> ZipList [21..30] -- [22,24,26,28,30,32,34,36,38,40]
```

### sequence
モナド版と同じ。こういうケースで便利 http://tanakh.jp/posts/2012-02-22-list-monad.html

あとこんなケースとか https://gist.github.com/taiki45/9210900

```haskell
sequence :: Monad m => [m a] -> m [a]
sequence $ [Just 3, Just 5] -- Just [3,5]
sequence $ [Just 3, Nothing] -- Nothing
```

### Alternative
`Alternative` は失敗が表現できる計算のための型クラスです。

基本的には失敗を表現する `empty` と計算をつなぎ合わせる `<|>` 演算子で構成されます。`<|>` の計算のつなげ合わせ方は**選択**です。

さらに「何度も繰り返し適用することで、成功から失敗へと状態を遷移させる」性質を持つような計算の場合、`some`, `many` メソッドが活用できます。`many`,`some` は計算が失敗するまで計算を繰り返し行い結果をリストにためこみます。失敗した時点で成功した結果のリストを返します。`many` の場合は初回の計算に失敗しても空リストを返します。対して `some` は初回の失敗に対しては `empty` を返します。

まずは、`Maybe` で実践してみます。

```haskell
Just 4 <|> Just 3 -- Just 4
Nothing <|> Just 3 -- Just 3

some $ Just 3 -- 無限ループ
many $ Nothing -- Just []
some $ Nothing -- Nothing
```

Maybe の成功値に対する `some`, `many` は何度繰り返しても成功になるので計算が無限に試行されます。

次は List で試してみましょう。リストの文脈での失敗は空リストです。

```haskell
[] <|> [1..5] -- [1,2,3,4,5]
[1..5] <|> [1..5] -- [1,2,3,4,5,1,2,3,4,5]

some [1..5] -- 無限ループ
```

List も成功を何度繰り返しても失敗することはありません。

下記の演習で確かめますが、`some`, `many` が活用できるのはパーサのような計算の場合です。

## Functor や Applicative の意義
あらゆるファンクターやアプリカティブファンクターやモナドは重要な操作以外を裏に隠してしまえるただの**便利パターン**なのです。

## 演習
Appicative Style といえばパーサー！パーサーコンビネータライブラリを作ってそれを Applicative Functor に仕立て上げ、Applicative Style で書けるようにしましょう！

>コンビネータとは自由項を持たない関数というぐらいの意味であるが、
ここではパーザを組み合わせてパーザを組み立てるような関数のことである。

>パーザコンビネータはプリミティブパーザとパーザ同士を組み合わせるコンビネータとからなる。

>小さいパーザを組み立てて最終的なパーザを作り上げるのである。

パーサーコンビネータライブラリの作り方 -> http://d.hatena.ne.jp/tanakh/20040731

ポイント: 最初は `let` や `where` でとりあえず結果や式を留めたりパタンマッチしておいて、後でリファクタリングするやりかたがやりやすいです。※個人の感想

実際に実装すると、トークンの消費状態や失敗の取り回し、といったパーサファンクター・パーサモナドが行っている裏配管の仕事がよくわかると思います。あらゆるファンクターやアプリカティブファンクターやモナドはこのように重要な操作以外を裏に隠してしまえるただの**便利パターン**なのです。

### Step1
`listOf` まで実装しましょう。

* 最初の `import Data.Char` をしておきます。`isDigit` などが使えます。
* `<|>` の実装でパースエラーが出る場合は2引数をとり無名関数を返すような実装しましょう。
* 参考コードでは  `symbol` を `char` としています。

```
ghci > natural "234a"
[(234,"a")]
```

### Step2
`Parser` の宣言を `type` 宣言から `newtype` 宣言に変えましょう。data constructor が必要になるので今までの一枚被さる形になります。ということはそのままだとパーサに文字列を渡すことができません。

そこで、data constructor を取り外しパーサ函数に引数を渡す `parse` のような補助函数を定義しましょう。

```haskell
parse :: Parser s a -> [s] -> [(a,[s])]
```

簡単ないくつかのコンビネータを新しい `Parser` 型を使って定義しなおしましょう。他の函数はとりあえずコメントアウトしましょう。

### Step3
`Parser` 型を Functor にしましょう。ついでに `parse` 函数もレコード構文で `Parser` 型に埋め込めるのでやってしまいましょう。

目標

```haskell
parse (succ `fmap` char 'c') "ceb" -- [('d',"eb")]
```

参考: `parse` 函数のように data constructor を取り外すとうまくいきますよ。

### Step4
`Parser` 型を Applicative のインスタンスにしましょう。

まず `Control.Applicative` を import する必要があります。今のままでは 同じ名前の函数があり、コンフリクトしてしまうので、一旦今までの実装はコメントアウトでもしましょう。Applivative のインスタンスにすると今まで実装した函数と等価な函数がメソッドとして手に入ると思います。どの函数とどのメソッドが対応するか考えてみてください。

目標

```haskell
parse ((:) <$> char 'c' <*> pure []) "ceb" -- [("c","ceb")]
parse (char 'c' *> pure 0) "ceb" -- [(0,"ceb")]
```

### Step5
`Parser` 型を Alternative のインスタンスにしましょう。 ref: http://itpro.nikkeibp.co.jp/article/COLUMN/20120110/378061/?ST=develop&P=4

`some` と `many` はデフォルト定義でもいけますが、練習として定義を与えてみてもよいですね。

目標

```haskell
natural :: Parser Char Integer
natural = read <$> some (satisfy isDigit)

parse ((*100) <$> natural) "456agd" -- [(45600,"agd")]
```

### Step6
`(+ 3 4)`あたりの簡単なS式をパースするパーサを実装してしまいましょう。

まず、S式を表すデータ型を作成しましょう。アトム型(シンボルを表す)、数値型、文字列型、リスト型くらいがあれば良さそうに思えます。

参考実装

```haskell
data Value = Atom String
           | Number Integer
           | String String
           | List [Value]
           deriving Show
```

Applicative の `<*` メソッドなどを活用したり新たなコンビネータを作成したりしましょう。

気が向いたら `eval` を実装してもよいですね。

#### 目標

```haskell
parse (parseExpr) "(+ 3 4)" -- [(List [Atom "+",Number 3,Number 4],"")]
```

#### 道筋
* 与えられた文字のどれかのマッチするようなコンビネータを定義する。`oneOf :: Eq s => [s] -> Parser s s`
* アルファベット全てにマッチする `alphabet :: Parser Char Char` を定義
* 与えられた文字のどれにもマッチしないようなコンビネータを定義する。 `noneOf :: Eq s => [s] -> Parser s s`
* 数値リテラルのパーサ `parseNumber :: Parser Char Value` を定義
* 文字列リテラルのパーサ `parseString :: Parser Char Value` を定義
* アトムのパーサ `parseAtom :: Parser Char Value` を定義
* 以前に実装した `(<&>) :: Parser s a -> Parser s b -> Parser s (a,b)` を新 `Parser` 向けに定義する。
* 以前に実装した `listOf :: Parser s a -> Parser s b -> Parser s [a]` を再定義する。
* 括弧の内側をパースするパーサ `parseList :: Parser Char Value` を定義する。
* S式をパースする `parseExpr :: Parser Char Value` を定義する。

### Step99
モナドを学習し終えたら `Parser` 型を `Monad` のインスタンスにしてみましょう。以前の `<&>` オペレーターが参考になります。型は `Parser s a -> (a -> Parser s b) -> Parser s b` になります。

実は `listOf` はモナドの力がないと実装できませんでした。そのことをより理解するために `listOf` を `>>=` オペレーターもしくは do 構文を使って実装しなおしてみましょう。

ほぼ答えは今までの実装にありますが、一点だけ以下のようなリストモナドの失敗に対する性質と do 構文を使うとスッキリ書けると思います。

```haskell
f n | n > 10 = [n]
    | otherwise = []

-- a = [(110,12)]
a = do n1 <- f 11
       let x = n1 * 10
       n2 <- f 12
       return (x,n2)

-- b = []
b = do n1 <- f 9
       let x = n1 * 10
       n2 <- f 12
       return (x,n2)
```