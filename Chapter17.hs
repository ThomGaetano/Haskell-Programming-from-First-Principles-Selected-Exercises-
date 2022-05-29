--chapter17.hs

module Chapter17 where

import Control.Applicative

data Pair a = Pair a a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Three' a b = Three' a b b deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Two a) where
   fmap f (Two a b) = Two a (f b)

instance Functor Pair where
   fmap f (Pair a a') = Pair (f a) (f a')

instance Functor (Three a b) where
   fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
   fmap f (Three' x y z) = Three' x (f y) (f z)

instance Functor (Four a b c) where
   fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four' a) where
   fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Applicative Pair where
   pure x = Pair (x) (x)
   (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Monoid a => Applicative (Two a) where
   pure x = Two mempty x
   (<*>) (Two f g) (Two x y) = Two (f <> x) (g y)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
   pure x = Three mempty mempty x
   (<*>) (Three f g h) (Three x y z) = Three (f <> x) (g <> y) (h z)

instance Monoid a => Applicative (Three' a) where
   pure x = Three' mempty x x
   (<*>) (Three' f g h) (Three' x y z) = Three' (f <> x) (g y) (h z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
   pure x = Four mempty mempty mempty x
   (<*>) (Four f g h k) (Four x y z l) = Four (f <> x) (g <> y) (h <> z) (k l)

instance Monoid a => Applicative (Four' a) where
   pure x = Four' mempty mempty mempty x
   (<*>) (Four' f g h k) (Four' x y z l) = Four' (f <> x) (g <> y) (h <> z) (k l)

u = Pair (+1) (+2)
v = Pair (+2) (+3)
w = Pair 1 2

pL = (pure (.)) <*> u <*> v <*> w
pR = u <*> (v <*> w)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos stops vowels stops' = liftA3 (,,) stops vowels stops'