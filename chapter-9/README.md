Chapter 9
=========

## 入出力
### 便利関数
* `getContents` - Ruby の `STDIN.read` っぽさ。遅延 I/O なので必要になる毎にメモリに読み込む。
* `interact` - 与えられた文字列を処理するような文字列処理関数 `String -> String` を受け取って、`getLine` した結果の `String` をその文字列処理関数に与え、文字列処理関数の返り値を `putStrLn` するような IO アクションを返す。よってこの関数のシグネチャは `(String -> String) -> IO ()`。

```
interact f = do input <- getLine
                putStrLn $ f input
```

### ファイルハンドラを使う関数
今まで登場した IO 関数たちはだいたいハンドラを使う版が定義されている。`System.IO` にある。`hPutStrLn` や `hGetLine` のような命名規則。ghci 上で `import System.IO` した後に `:t System.IO.h<tab>` とするといろいろ見えると思います。`hSeek` とかあるので人によっては seek とか…ウッ

* `openFile :: FilePath -> IOMode -> IO Handle` - ファイルパスとモードが与えられるとファイルハンドラを返す。
* `hClose :: Handle -> IO ()` - 与えられたファイルハンドラのファイルを閉じる。
* `IOMode` - ファイルのモードを表す列挙型(直和型)。`data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode`。
* `openTempFile :: FilePath -> String -> IO (FilePath, Handle)` - 1.一時ファイルを作るディレクトリへのパス、2.ファイル名の prefix 文字列、を受け取り一時ファイルのファイルパスとハンドラを返す。2番目の引数に `temp` という文字列を渡すと実際のファイル名は `temp72359` のようにランダムな文字列がつく。

### System.Directory の関数
便利そう。

* `removeFile :: FilePath -> IO ()`
* `renameFile :: FilePath -> FilePath -> IO ()`

### エラーに備える系関数
* `withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r` - 1.ファイルパス、2.モード、3.ハンドラを取って結果を返す(`r` は結果の型)関数、の3引数を取って結果(`r`型)を返すような IO アクションをつくります。3つめの実際に処理する関数実行中にエラーになってもハンドラをちゃんとしょりするための関数です。エラーの例としてはパターンマッチミスやリストへの存在しない index アクセスなどがあります。
* `bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c` - `withFile` を一般化した関数。1.IO に包まれたリソース、2.リソースの解放を行う関数、3.リソースを使ってなにか行う関数、の3引数を取って3つめのリソースを使って処理した結果を返すような IO アクションをつくります。
* `bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c` - `bracket` は処理が終わると常にリソースの解放処理をするが、この関数はエラーが起きた時だけリソース解放処理を呼ぶ。

### 一度に処理やっちゃう系関数
* `readFile :: FilePath -> IO String` - ファイルから読み込んで内容を IO に包まれた `String` として返す。
* `writeFile :: FilePath -> String -> IO ()` - ファイルパスと内容を `String` として渡すと書き込んでくれる。
* `appendFile :: FilePath -> String -> IO ()` - アッペンド！！！！

### コマンドライン引数
`System.Environment` にそろってます。ちなみに ghci 上で main を任意のコマンドライン引数付きで走らせるには `ghci> :main foo bar` とします。

* `getArgs :: IO [String]` - 各引数が `String` で入っている。引数がひとつもない時はご想像の通り空リストです。

## 片手にチーズ、もう片方に風船を持って一輪車に乗ったサル
`System.Random` にそろってます。`Random` 型クラスのインスタンスにすることで任意の型をランダムに出力できるようになる仕組み。

### ランダム
`random :: (RandomGen g, Random a) => g -> (a, g)` のようにジェネレーターを引き回すことで純粋さとランダム性を両立させています。ここの `Random a` に具体的な型があてはまります。例えば `Int` や `Bool`。

いくつかのランダムな値が欲しい場合は `randoms :: (RandomGen g, Random a) => g -> [a]` を使う。例えば十回コイントスした結果っぽさを表す:

```
-- この式だけだと推論できないので型注釈を与える
take 10 $ randoms (mkStdGen 11) :: [Bool]
```

ある範囲のランダムな値がほしい場合は `randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)` を使う。第一引数で範囲のペアを (上限, 下限) で渡す。さらに、ある範囲の複数のランダムな値がほしい場合は `randomRs` を使う。

```
take 10 $ randomRs (1, 100) (mkStdGen 11) :: [Int]
```

### もっとランダム
純粋な世界だと固定した乱数ジェネレーターしかつかえないので、Haskell の外の世界からなにかランダム性のあるデータを持ってくる必要があります。外の世界、IO の世界ですね。

```
main = do
    gen <- getStdGen
    print $ take 10 $ (randomRs (0, 99) gen :: [Int])
```

ジェネレーターを手で作るのではなくゆらぎある外の世界の情報を使って作ることでよりランダムな感じになります。

## bytestring / Text
Haskell の文字列は `[Char]`、つまりリストです。さらにデフォルト遅延評価なのでリストはサンクの塊になってます。なので大きな文字列を扱うには不向き。

ということでバイト列をそのまま扱える bytestring の出番なのですが、マルチバイト文字を扱うことを考えると bytestring でそのまま文字列処理するのはあまりにも過酷っ…！

そこで Text ライブラリです(`Data.Text`)。現在の内部実装はUTF-16でエンコードされた16ビット列で保持する。しかし API としてはユニコード文字列をそのまま扱う感じになっているので意識しなくても良い、とのこと。

さらにソースコード上の即値文字列を多相的にする GHC 拡張があります。それが **OverloadedStrings** 拡張です。

```
$ ghci
> import Data.Text
> let isA :: Text -> Bool; isA "あ" = True; isA _ = False
<interactive>:7:30:
    Couldn't match expected type `Text' with actual type `[Char]'
    In the pattern: "\12354"
    In an equation for `isA': isA "\12354" = True

> :set -XOverloadedStrings
> :t "あ"
あ" :: Data.String.IsString a => a
> let isA :: Text -> Bool; isA "あ" = True; isA _ = False
> isA "a"
False
> isA "あ"
True
```

そして…

```
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do txt <- T.getLine
          T.putStrLn $ T.append "ε( ε^o^)э " txt
```

こんな感じで Text 用の IO 関数を使えば `String` か `Text` かとあまり意識しないでも書けるようになっています。さらに Text から String も自在にできます。`Data.Text unpack :: Text -> String`。



## 演習1. 強力なパスワードプログラムを手続き型っぽく書こう！
超強力6ケタ数字パスワードプログラムぅ〜〜

```
$ runghc 1-exercise.hs
What is password?
> 123456
Wrong!!
What is password?
> 654321
Got it!!!

* * * * * * * * * * * * * * * * * * *
you know L, reapers eat only apples...
```

### ステップ1
失敗したらもう一度繰り返したいです〜。
Haskell では繰り返し処理は再帰を使うといいですよ！

じゃあ、条件をとって繰り返しを行う `while :: a -> (a -> Bool) -> IO a -> IO a` のような 関数があったら便利じゃないですか…？
第一引数に**なにかを受け取って `Bool` を返す関数**(述語)受け取って、第二引数に失敗した時に行う IO アクション(while の body)をうけとればいいんじゃないかな！ body の実行結果をまた述語に渡しましょう！

こうすることで状況によって膨れ上がる大きな IO アクションをつくることができます。IO アクションが膨れ上がっても `while` の型は結局 `IO a` なので問題ないですね！

### ステップ2
`while` ができたら `while` ループが終わった後になにをするか考えてみましょう！
認証が成功した時にはいろいろ表示する必要があるますね。
わぁ！これって IO アクションです！

IO アクションとIOアクションをつなげるにはどうすれば…？


## 演習2. パスワード入力機能をつけて、パスワードファイルを読もう！
やれやれ、パスワードがソースコードに埋め込んであるなんて強力なパスワードプログラムとはいえないですね。
ユーザーがコマンドラインでパスワードを入力して保存できるようにしましょう！

**もちろん引数がなかったり、引数が違う場合はヘルプを表示します！**

```
$ runghc 2-exercise.hs
=Usage=
lock:
    passwordProg.hs lock 'your password'
open:
    passwordProg.hs open
$ runghc passwordProg.hs lock 'password'
$ runghc passwordProg.hs open
What is password?
> 123456
Wrong!!
What is password?
> password
Got it!!!

* * * * * * * * * * * * * * * * * * *
you know L, reapers eat only apples...
```

### ステップ1
分岐には `case of` とガードもつかえます。
パターンマッチしたい時は `case of`、`Bool` で分岐したいときはガードです！

### ステップ2
ファイルに書かれたパスワードを読み出す IO は一度だけになるようにしましょう！先にパスワードを取り出して、`String` として与えましょう。


## 演習3. パスワード生成機能をつけよう！
ユーザーにパスワードを考えさせると "abcd" とかになってしますので危険です。
ここは親切に良さそうなパスワードを設定できるように書き換えましょう！

```
$ runghc 3-exercise.hs lock
$ cat password.txt
P;@>5YA8LP
$ runghc 3-exercise.hs open
What is password?
> 123456
Wrong!!
What is password?
> P;@>5YA8LP
Got it!!!

* * * * * * * * * * * * * * * * * * *
you know L, reapers eat only apples...
```

### ステップ1
`getStdGen :: IO StdGen` 関数でジェネレーターを作りましょう！

 `randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]` 関数でランダムな文字列を作ると OK ですね！Char で範囲を指定するとうまくいきますよ！