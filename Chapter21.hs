--chapter21.hs

{-# LANGUAGE FlexibleContexts #-}

module Chapter21 where

import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative

newtype Identity a = Identity a deriving (Eq, Ord, Show)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

data Optional a = Nada | Yep a deriving (Eq, Show)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Big a b = Big a b b deriving (Eq, Show)

data Bigger a b = Bigger a b b b deriving (Eq, Show)

data S n a = S (n a) a deriving (Eq, Show)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

instance Functor Identity where
   fmap f (Identity a) = Identity (f a)

instance Functor (Constant a) where
   fmap _ (Constant x) = Constant x

instance Functor Optional where
   fmap _ Nada = Nada
   fmap f (Yep a) = Yep (f a)

instance Functor List where
   fmap _ Nil = Nil
   fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Functor (Three a b) where
   fmap f (Three a b c) = Three a b (f c)

instance Functor (Two a) where
   fmap f (Two a b) = Two a (f b)

instance Functor (Big a) where
   fmap f (Big a b b') = Big a (f b) (f b')

instance Functor (Bigger a) where
   fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Functor n => Functor (S n) where
   fmap f (S x y) = S (fmap f x) (f y)

instance Functor Tree where
   fmap _ Empty = Empty
   fmap f (Leaf a) = Leaf (f a)
   fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Applicative Identity where
   pure = Identity
   (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monoid a => Applicative (Constant a) where
   pure x = Constant mempty
   (<*>) (Constant f) (Constant x) = Constant f

instance Applicative Optional where
   pure = Yep
   (<*>) Nada _ = Nada
   (<*>) _ Nada = Nada
   (<*>) (Yep f) (Yep x) = Yep (f x)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
   pure = Three mempty mempty
   (<*>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c c')

instance Monoid a => Applicative (Two a) where
   pure = Two mempty
   (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

instance Monoid a => Applicative (Big a) where
   pure x = Big mempty x x
   (<*>) (Big a b c) (Big d e f) = Big (a <> d) (b e) (c f)

instance Monoid a => Applicative (Bigger a) where
   pure x = Bigger mempty x x x
   (<*>) (Bigger a b c x) (Bigger d e f y) = Bigger (a <> d) (b e) (c f) (x y)

instance Applicative n => Applicative (S n) where
   pure x = S (pure x) x
   (<*>) (S f g) (S x y) = S (f <*> x) (g y)

instance Applicative Tree where
   pure = Leaf
   (<*>) Empty _ = Empty
   (<*>) _ Empty = Empty
   (<*>) (Leaf a) (Leaf b) = Leaf (a b)
   (<*>) (Node left a right) (Node left' a' right') = Node (left <*> left') (a a') (right <*> right')
   (<*>) (Leaf a) (Node left a' right) = Node (Leaf a <*> left) (a a') (Leaf a <*> right)
   (<*>) (Node left a right) (Leaf a') = Node (left <*> Leaf a') (a a') (right <*> Leaf a')

instance Monad Identity where
   return = pure
   (>>=) (Identity a) f = f a

instance Monoid a => Monad (Two a) where
   return = pure
   (>>=) (Two a b) f = (Two a id) <*> (f b)

instance Foldable Identity where
   foldr f z (Identity a) = f a z
   foldMap f (Identity a) = f a

instance Foldable (Constant a) where
   foldr f z (Constant a) = z
   foldMap f (Constant a) = mempty

instance Foldable Optional where
   foldr f z Nada = z
   foldr f z (Yep a) = f a z
   foldMap f Nada = mempty
   foldMap f (Yep a) = f a

instance Foldable List where
   foldr f z Nil = z
   foldr f z (Cons a b) = (f a) (foldr f z b)
   foldMap f Nil = mempty
   foldMap f (Cons a b) = (f a) <> (foldMap f b)

instance Foldable (Three a b) where
   foldr f z (Three a b c) = f c z
   foldMap f (Three a b c) = f c

instance Foldable (Two a) where
   foldr f z (Two a b) = f b z
   foldMap f (Two a b) = f b

instance Foldable (Big a) where
   foldr f z (Big a b b') = f b' (f b z)
   foldMap f (Big a b b') = (f b) <> (f b')

instance Foldable (Bigger a) where
   foldr f z (Bigger a b b' b'') = f b'' (f b' (f b z))
   foldMap f (Bigger a b b' b'') = (f b) <> (f b') <> (f b'')

instance Foldable n => Foldable (S n) where
   foldr f z (S x y) = undefined
   foldMap f (S x y) = (foldMap f x) <> (f y)

instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf a) = (f a)
   foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

   foldr f z Empty = z
   foldr f z (Leaf a) = f a z
   foldr f z (Node left a right) = foldr f (f a (foldr f z right)) (left)

instance Traversable Identity where
   sequenceA (Identity a) = Identity <$> a
   traverse f (Identity a) = Identity <$> (f a)

instance Traversable (Constant a) where
   sequenceA (Constant a) = pure (Constant a)
   traverse f (Constant a) = pure (Constant a)

instance Traversable Optional where
   sequenceA Nada = pure Nada
   sequenceA (Yep a) = Yep <$> a

instance Traversable List where
   sequenceA Nil = pure Nil
   sequenceA (Cons a b) = (Cons <$> a) <*> (sequenceA b)
   traverse _ Nil = pure Nil
   traverse f (Cons a b) = (Cons <$> (f a)) <*> (traverse f b)

instance Traversable (Three a b) where
   sequenceA (Three a b c) = (Three a b) <$> c
   traverse f (Three a b c) = (Three a b) <$> (f c)

instance Traversable (Two a) where
   sequenceA (Two a b) = (Two a) <$> b
   traverse f (Two a b) = (Two a) <$> (f b)

instance Traversable (Big a) where
   sequenceA (Big a b b') = ((Big a) <$> b) <*> b'
   traverse f (Big a b b') = ((Big a) <$> (f b)) <*> (f b')

instance Traversable (Bigger a) where
   sequenceA (Bigger a b b' b'') = ((Bigger a) <$> b) <*> b' <*> b''
   traverse f (Bigger a b b' b'') = ((Bigger a) <$> (f b)) <*> (f b') <*> (f b'')

instance Traversable n => Traversable (S n) where
   traverse f (S x y) = (S <$> traverse f x) <*> (f y)

instance Traversable Tree where
   traverse f Empty = pure Empty
   traverse f (Leaf a) = Leaf <$> (f a)
   traverse f (Node left a right) = (Node <$> (traverse f left)) <*> (f a) <*> (traverse f right)