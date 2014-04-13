# 14章 もうちょっとだけモナド
memo: Control.Monad を Control.Applicative を import しておきましょう。

## 14.4 Error を壁に
Maybe も Either も失敗を表せる。Maybe は失敗した計算の以降の計算を全てスキップすることによって、失敗を表現している。Either は同じように全てスキップするが、値と共にスキップできる。

Right で成功 + 値を、Left で失敗 + 値を持つ。

Functor として考えるとわかりやすい。Left value は mapping されない、Right value は mapping される。なので、Left の場合は一切の計算が mapping されず Right の場合は以降の計算が mapping される。このようにしてエラー処理を実現している。

```haskell
let l = Left "abc"
let r = Right 5

fmap (+4) l

fmap (+4) r
```

TODO: 現在の実装 p342

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

誤解を恐れずにいうと、モナドというパターンは join を持った Functor を考えましょう、ということです。join は Functor の重なりを平らにする関数です。これがあれば bind (>>=) がなくても上のような問題は解決できます！

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
