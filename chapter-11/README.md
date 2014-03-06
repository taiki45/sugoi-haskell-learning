# 11章

```
Functor ∋ Applicative Functor ∋ Monad
```

実装上は歴史的経緯と考慮すべき事情により上図のようになっていないが、全てのモナドはファンクターである。実装上もほとんどのモナドはファンクターなのでこれから紹介する便利な機能や性質が使える。

## Functor

* ファンクターは箱あるいは文脈付きの値、あるいは**計算**。
* 「X がファンクターである」と言えることは「X は函数で写せるなにがである」ということ。
* `fmap` は函数の持ち上げと適用を行う。

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

```
fmap (id :: a -> a) = (id :: Functor f => f a -> f a)
```

※ `id` 函数は多相的である

```
fmap (id :: Int -> Int) $ Just 4 = (id :: Functor f => f Int -> f Int) $ Just 4
```

### 2
ファンクターF, 型a, b, c, 函数 f: a -> b, g: b -> c について、(F g) ○ (F f) = F (g ○ f)

```
f x = x + 1
g x = x * 5
(fmap g) . (fmap f) $ Just 5 = Just $ (g . f) 5
```

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

```
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

```
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

```
Just 5 *> pure 0 -- Just 0
pure "fail" <* getLine -- IO "fail"
```

### アプリカティブ則
TODO: いくつかのアプリカティブファンクターで証明

### ZipList
`zipWithN` の抽象。

```
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

-- 以下は等価
zipWith (+) [1..10] [21..30] -- [22,24,26,28,30,32,34,36,38,40]
getZipList $ (+) <$> ZipList [1..10] <*> ZipList [21..30] -- [22,24,26,28,30,32,34,36,38,40]
```

### sequence
モナド版と同じ。こういうケースで便利 http://tanakh.jp/posts/2012-02-22-list-monad.html

あとこんなケースとか https://gist.github.com/taiki45/9210900

```
sequence :: Monad m => [m a] -> m [a]
sequence $ [Just 3, Just 5] -- Just [3,5]
sequence $ [Just 3, Nothing] -- Nothing
```

### Altenative
TODO

## Functor や Applicative の意義


## 演習
Appicative Style といえばパーサー！パーサーコンビネータライブラリを作ってそれを Applicative Functor に仕立て上げ、Applicative Style で書けるようにしましょう！

パーサーコンビネータライブラリの作り方 -> http://d.hatena.ne.jp/tanakh/20040731

ポイント: 最初は `let` や `where` でとりあえず結果や式を留めたりパタンマッチしておいて、後でリファクタリングするやりかたがやりやすいです。※個人の感想

実際に実装すると、トークンの消費状態や失敗の取り回し、といったパーサファンクター・パーサモナドが行っている裏配管の仕事がよくわかると思います。あらゆるファンクターやアプリカティブファンクターやモナドはこのように重要な操作以外を裏に隠してしまえるただの**便利なパターン**なのです。

### Step1
`listOf` まで実装しましょう。

参考コードでは  `symbol` を `char` としています。

### Step2
下記のような `number` パーサを実装しましょう。

```
digit :: Parser Char Char
digit = satisfy isDigit

number :: Parser Char Integer
number = read <$> many1 digit
```

```
ghci > number "234a"
[(234,"a")]
```

### Step3
`Parser` の宣言を `type` 宣言から `newtype` 宣言に変えましょう。data constructor が必要になるので今までの一枚被さる形になります。ということはそのままだとパーサに文字列を渡すことができません。

そこで、data constructor を取り外しパーサ函数に引数を渡す `parse` のような補助函数を定義しましょう。

```
parse :: Parser s a -> [s] -> [(a,[s])]
```

### Step4
`Parser` 型を Functor にしましょう。

参考: `parse` 函数のように data constructor を取り外すとうまくいきますよ。

### Step5
`Parser` 型を Applicative のインスタンスにしましょう。

まず `Control.Applicative` を import する必要があります。今のままでは 同じ名前の函数があり、コンフリクトしてしまうので、一旦今までの実装はコメントアウトでもしましょう。Applivative のインスタンスにすると今まで実装した函数と等価な函数がメソッドとして手に入ると思います。どの函数とどのメソッドが対応するか考えてみてください。

### Step6
`Parser` 型を Alternative のインスタンスにしましょう。 ref: http://itpro.nikkeibp.co.jp/article/COLUMN/20120110/378061/?ST=develop&P=4

`some` と `many` はおそらくデフォルト定義だと無限ループに陥ってしまうので、それぞれ定義を与えてあげましょう。

### Step7
`(+ 3 4)`あたりの簡単なS式をパースするパーサを実装してしまいましょう。

まず、S式を表すデータ型を作成しましょう。アトム型(シンボルを表す)、数値型、文字列型、リスト型くらいがあれば良さそうに思えます。

Applicative の `<*` メソッドなどを活用したり新たなコンビネータを作成したりしましょう。

気が向いたら `eval` を実装してもよいですね。

### Step8
モナドを学習し終えたら `Parser` 型を `Monad` のインスタンスにしてみましょう。

ほぼ答えは今までの実装にありますが、一点だけ以下のようなリストモナドの失敗に対する性質と do 構文を使うとスッキリ書けると思います。

```
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