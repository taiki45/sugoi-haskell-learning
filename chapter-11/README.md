# 11章

```
Functor ∋ PointedFunctor ∋ Applicative Functor ∋ Monad
```

実装上は歴史的経緯と考慮すべき事情により上図のようになっていないが、全てのモナドはファンクターである。だから「IO 型はファンクターである」は変じゃないんですよ。

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
* 最小のファンクター値を作る函数 `pure` を備えている。
* 包まれた or 持ち上げられた函数にファンクター値で適用できる函数`<*>`を備えている。

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


### アプリカティブ則
TODO: いくつかのアプリカティブファンクターで証明

### ZipList

### sequence

### Altenative
TODO

## 演習
Appicative Style といえばパーサー！パーサーコンビネータライブラリを作ってそれを Applicative Functor に仕立て上げ、Applicative Style で書けるようにしよう！

パーサーコンビネータライブラリの作り方 -> http://d.hatena.ne.jp/tanakh/20040730