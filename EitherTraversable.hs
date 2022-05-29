--eitherTraversable.hs

module EitherTraversable where

import Data.Traversable
import Data.Foldable
import Data.Monoid

data Validation a b = LeftX a | RightX b deriving (Eq, Show)

instance Functor (Validation a) where
   fmap _ (LeftX a) = LeftX a
   fmap f (RightX b) = RightX (f b)

instance Applicative (Validation a) where
   pure = RightX
   (<*>) (LeftX a) _ = LeftX a
   (<*>) _ (LeftX a) = LeftX a
   (<*>) (RightX a) (RightX b) = a <$> (RightX b)

instance Foldable (Validation a) where
   foldr _ z (LeftX a) = z
   foldr f z (RightX a) = f a z

   foldMap _ (LeftX a) = mempty
   foldMap f (RightX a) = f a

instance Traversable (Validation a) where
   sequenceA (LeftX a) = pure (LeftX a)
   sequenceA (RightX a) = RightX <$> a

   traverse _ (LeftX a) = pure (LeftX a)
   traverse f (RightX a) = RightX <$> (f a)