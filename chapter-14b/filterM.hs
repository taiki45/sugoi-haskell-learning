module FilterM where

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do result <- p x
                       if result
                           then do xs' <- filterM' p xs
                                   return $ x:xs'
                           else filterM' p xs
