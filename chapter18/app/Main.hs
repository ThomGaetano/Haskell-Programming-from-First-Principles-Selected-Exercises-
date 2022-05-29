module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad ((>=>), join)
import Control.Applicative

data CountMe a = CountMe Integer a deriving (Eq, Show)

data Nope a = NopeDotJpg deriving (Eq, Show)

data EitherX b a = LeftX a | RightX b deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor Identity where
   fmap f (Identity a) = Identity (f a)

instance Functor (EitherX b) where
   fmap _ (RightX b) = RightX b
   fmap f (LeftX a) = LeftX (f a)

instance Functor Nope where
   fmap _ NopeDotJpg = NopeDotJpg

instance Functor CountMe where
   fmap f (CountMe i a) = CountMe i (f a)

instance Functor List where
   fmap _ Nil = Nil
   fmap f (Cons a x) = Cons (f a) (fmap f x)

instance Applicative Identity where
   pure x = Identity x
   (<*>) (Identity f) (Identity x) = Identity (f x)

instance Applicative (EitherX b) where
   pure x = LeftX x
   (<*>) _ (RightX b) = RightX b
   (<*>) (RightX b) _ = RightX b
   (<*>) (LeftX a) (LeftX b) = LeftX (a b)

instance Monad (EitherX b) where
   return = pure
   (>>=) (RightX a) _ = RightX a
   (>>=) (LeftX a) f = (f a)

instance Applicative Nope where
   pure x = NopeDotJpg
   (<*>) f g = NopeDotJpg

instance Applicative CountMe where
   pure = CountMe 0
   CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Applicative List where
   pure x = Cons x Nil
   (<*>) _ Nil = Nil
   (<*>) Nil _ = Nil
   (<*>) (Cons f g) (Cons x y) = Cons (f x) (pure f <*> y) `append` (g <*> Cons x y)

instance Monad Nope where
   return = pure
   (>>=) f g = NopeDotJpg

instance Monad CountMe where
   return = pure
   CountMe n a >>= f = let CountMe n' b = (f a)
                       in CountMe (n + n') b

instance Monad Identity where
   return = pure
   (>>=) (Identity a) f = (f a)

instance Monad List where
   return = pure
   (>>=) Nil _ = Nil
   (>>=) (Cons a x) f = (f a) `append` (x >>= f)

instance Arbitrary a => Arbitrary (CountMe a) where
   arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
   (=-=) = eq

instance Arbitrary a => Arbitrary (Nope a) where
   arbitrary = do
           x <- arbitrary
           return x 

instance Eq a => EqProp (Nope a) where
   (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append ys Nil = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

join' :: Monad m => m (m a) -> m a
join' = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f xs = f <$> xs

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f xs ys = f <$> xs <*> ys

a :: Monad m => m a -> m (a -> b) -> m b
a xs fs = fs <*> xs

meh :: (Semigroup (m [b]), Monad m) => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = ((f x) >>= (\x -> return [x])) <> meh xs f

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] _ = return []
meh' (x:xs) f = ((:) <$> (f x)) <*> (meh' xs f)

flipType :: (Semigroup (m [a]), Monad m) => [m a] -> m [a]
flipType [] = return []
flipType (x:xs) = (x >>= (\x -> return [x])) <> (flipType xs)

flipType' :: Monad m => [m a] -> m [a]
flipType' xs = meh' xs id

main :: IO ()
main = do
   let trigger :: CountMe (Int, String, Int)
       trigger = undefined
   quickBatch $ functor trigger
   quickBatch $ applicative trigger
   quickBatch $ monad trigger

