module While
    ( while
    , doWhile )
where

while :: a -> (a -> Bool) -> IO a -> IO a
while a f body = do if f a
                        then do result <- body
                                while result f body
                        else return a

doWhile :: (a -> Bool) -> IO a -> IO a
doWhile f body = do result <- body
                    if f result then doWhile f body
                                else return result
