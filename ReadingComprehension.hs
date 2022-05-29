--readingComprehension.hs

{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
   fmap :: (a -> b) -> Reader r a -> Reader r b
   fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
   pure :: a -> Reader r a
   pure a = Reader $ (const a)

   (<*>) :: Reader r (a -> b) 
         -> Reader r a -> Reader r b
   (<*>) (Reader rab) (Reader ra) = 
            Reader $ \r -> (rab r <$> ra) r

instance Monad (Reader r) where
   return = pure
   (>>=) (Reader ra) (rBa) = join (Reader $ \r -> rBa (ra r))

myLiftA2 :: Applicative f =>
           (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f xs ys = f <$> xs <*> ys

asks :: (r -> a) -> Reader r a
asks f = Reader f

foo :: Num a => [a] -> [a]
foo r = map (+1) r

bar :: Num a => [a] -> [a] -> ([a], Int)
bar r t = (r, length t)

genBind :: (t2 -> t1) -> (t1 -> t2 -> t) -> t2 -> t
genBind f g = \r -> g (f r) r

fooBind :: Num a => [a] -> ([a], Int)
fooBind = \r -> bar (foo r) r

fooBind' :: Num a => [a] -> ([a], Int)
fooBind' = \r -> (foo >>= bar) r