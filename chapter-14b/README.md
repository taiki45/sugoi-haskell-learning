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

