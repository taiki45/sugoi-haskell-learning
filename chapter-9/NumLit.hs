main = undefined

data MyInteger = MyInteger Integer
               deriving Show

instance Num MyInteger where
        (*) = undefined
        (+) = undefined
        abs = undefined
        signum = undefined
        fromInteger x = MyInteger x
    --  ^ これを実装すると数値リテラルを
    --    ユーザ定義型として扱うことができる！:
    --        *Main> (4 :: MyInteger)
    --        MyInteger 4
