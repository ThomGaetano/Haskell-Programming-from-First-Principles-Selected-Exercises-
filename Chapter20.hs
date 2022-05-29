--chapter20.hs

module Chapter20 where

import Data.Foldable
import Data.Monoid

data Constant a b = Constant b deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Three' a b = Three' a b b deriving (Eq, Show)

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Constant a) where
   foldr f z (Constant b) = f b z
   foldMap f (Constant b) = f b

instance Foldable (Two a) where
   foldr f z (Two a b) = f b z
   foldMap f (Two a b) = f b

instance Foldable (Three a b) where
   foldr f z (Three a b c) = f c z
   foldMap f (Three a b c) = f c

instance Foldable (Three' a) where
   foldr f z (Three' a b b') = f b' (f b z)
   foldMap f (Three' a b b') = (f b') <> (f b)

instance Foldable (Four' a) where
   foldr f z (Four' a b b' b'') = f b'' (f b' (f b z))
   foldMap f (Four' a b b' b'') = (f b'') <> (f b') <> (f b)

filterF :: (Applicative f, Foldable t, Monoid (f a)) 
        => (a -> Bool) -> t a -> f a
filterF g xs = foldMap (\x -> if ((g x) == True) then (pure x) else mempty) xs

