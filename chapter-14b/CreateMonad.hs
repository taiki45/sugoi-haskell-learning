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
