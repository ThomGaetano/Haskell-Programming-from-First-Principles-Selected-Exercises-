{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import GHC.Arr

functorId :: (Functor f, Eq (f a)) => f a -> Bool
functorId f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data Company a b c = DeepBlue a c | Something b deriving (Eq, Show)

data More a b = L a b a | R b a b deriving (Eq, Show)

data Quant a b = Finance | Desk a | Floor b deriving (Eq, Show)

data K a b = K a deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
   arbitrary = boolGen

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
   arbitrary = boolMaybeGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
   arbitrary = sumGen

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a b c) where
   arbitrary = compGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (More a b) where
   arbitrary = moreGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
   arbitrary = quantGen

instance Arbitrary a => Arbitrary (K a b) where
   arbitrary = kGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
   arbitrary = flipGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
   arbitrary = goatGen

instance Arbitrary a => Arbitrary (LiftItOut Maybe a) where
   arbitrary = liftGen

instance Arbitrary a => Arbitrary (Parappa Maybe Maybe a) where
   arbitrary = parGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe Maybe a b) where
   arbitrary = ignoreGen

instance Arbitrary a => Arbitrary (List a) where
   arbitrary = listGen

instance Arbitrary a => Arbitrary (GoatLord a) where
   arbitrary = lordGen

instance Functor BoolAndSomethingElse where
   fmap f (False' a) = False' (f a)
   fmap f (True' a) = True' (f a)

instance Functor BoolAndMaybeSomethingElse where
   fmap f Falsish = Falsish
   fmap f (Truish a) = Truish (f a)

instance Functor (Sum a) where
   fmap _ (First a) = First a
   fmap f (Second b) = Second (f b)

instance Functor (Company a b) where
   fmap _ (Something b) = Something b
   fmap f (DeepBlue a c) = DeepBlue a (f c)

instance Functor (More a) where
   fmap f (L a b c) = L a (f b) c
   fmap f (R x y z) = R (f x) y (f z)

instance Functor (Quant a) where
   fmap _ Finance = Finance
   fmap _ (Desk a) = Desk a
   fmap f (Floor b) = Floor (f b)

instance Functor (K a) where
   fmap _ (K a) = (K a)

instance Functor (Flip K a) where
   fmap f (Flip (K a)) = Flip $ K (f a)

instance Functor (EvilGoateeConst a) where
   fmap f (GoatyConst b) = GoatyConst (f b)

instance Functor f => Functor (LiftItOut f) where
   fmap g (LiftItOut (fa)) = LiftItOut (fmap g (fa))

instance (Functor f, Functor g) => Functor (Parappa f g) where
   fmap k (DaWrappa (fa) (ga)) = DaWrappa (fmap k (fa)) (fmap k (ga))

instance Functor g => Functor (IgnoreOne f g a) where
   fmap z (IgnoringSomething (fa) (gb)) = IgnoringSomething (fa) (fmap z (gb))

instance Functor List where
   fmap _ Nil = Nil
   fmap f (Cons a x) = Cons (f a) (fmap f x)

instance Functor GoatLord where
   fmap _ NoGoat = NoGoat
   fmap f (OneGoat a) = OneGoat (f a)
   fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance Functor TalkToMe where
   fmap _ Halt = Halt
   fmap f (Print x y) = Print x (f y)
   fmap f (Read z) = Read (fmap f z)

boolGen :: Arbitrary a => Gen (BoolAndSomethingElse a)
boolGen = do
   a <- arbitrary
   elements [False' a, True' a]

boolMaybeGen :: Arbitrary a => Gen (BoolAndMaybeSomethingElse a)
boolMaybeGen = do
   a <- arbitrary
   elements [Falsish, Truish a]

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
   a <- arbitrary
   b <- arbitrary
   elements [First a, Second b]

compGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Company a b c)
compGen = do
   a <- arbitrary
   b <- arbitrary
   c <- arbitrary
   elements [DeepBlue a c, Something b]

moreGen :: (Arbitrary a, Arbitrary b) => Gen (More a b)
moreGen = do
   a <- arbitrary
   b <- arbitrary
   elements [L a b a, R b a b]

quantGen :: (Arbitrary a, Arbitrary b) => Gen (Quant a b)
quantGen = do
   a <- arbitrary
   b <- arbitrary
   elements [Finance, Desk a, Floor b]

kGen :: Arbitrary a => Gen (K a b)
kGen = do
   a <- arbitrary
   return (K a)

flipGen :: (Arbitrary a, Arbitrary b) => Gen (Flip K a b)
flipGen = do
   a <- arbitrary
   return (Flip (K a))

goatGen :: (Arbitrary b) => Gen (EvilGoateeConst a b)
goatGen = do
   b <- arbitrary
   return (GoatyConst b)

liftGen :: Arbitrary a => Gen (LiftItOut Maybe a)
liftGen = do
   a <- arbitrary
   elements [LiftItOut Nothing, LiftItOut (Just a)]

parGen :: Arbitrary a => Gen (Parappa Maybe Maybe a)
parGen = do
   a <- arbitrary
   return (DaWrappa Nothing (Just a))

ignoreGen :: (Arbitrary a, Arbitrary b) => Gen (IgnoreOne Maybe Maybe a b)
ignoreGen = do
   a <- arbitrary
   b <- arbitrary
   elements [IgnoringSomething Nothing Nothing, IgnoringSomething Nothing (Just b), IgnoringSomething (Just a) Nothing]

listGen :: Arbitrary a => Gen (List a)
listGen = do
   a <- arbitrary
   elements [Nil, Cons a (Cons a Nil), Cons a (Cons a (Cons a Nil))]

lordGen :: Arbitrary a => Gen (GoatLord a)
lordGen = do
   a <- arbitrary
   elements [NoGoat, OneGoat a, MoreGoats (NoGoat) (OneGoat a) (MoreGoats (NoGoat) (OneGoat a) (OneGoat a))]

main :: IO ()
main = do
   quickCheck (functorId :: BoolAndSomethingElse Int -> Bool)
   quickCheck (functorCompose (+1) (*2) :: BoolAndSomethingElse Int -> Bool)
   quickCheck (functorId :: BoolAndMaybeSomethingElse Int -> Bool)
   quickCheck (functorCompose (+1) (*2) :: BoolAndMaybeSomethingElse Int -> Bool)
   quickCheck (functorId :: Sum Bool Int -> Bool)
   quickCheck (functorCompose (*2) (+3) :: Sum Bool Int -> Bool)
   quickCheck (functorId :: Company Ordering Double Int -> Bool)
   quickCheck (functorCompose (*9) (+12) :: Company Ordering Double Int -> Bool)
   quickCheck (functorId :: More Bool Int -> Bool)
   quickCheck (functorCompose (*3) (+3) :: More Bool Int -> Bool)
   quickCheck (functorId :: Quant Ordering Int -> Bool)
   quickCheck (functorCompose (+2) (*8) :: Quant Ordering Int -> Bool)
   quickCheck (functorId :: K Int Bool -> Bool)
   quickCheck (functorCompose (*2) (+1) :: K Int Int -> Bool)
   quickCheck (functorId :: Flip K Int Int -> Bool)
   quickCheck (functorCompose (*2) (+1) :: Flip K Int Int -> Bool)
   quickCheck (functorId :: EvilGoateeConst Bool Int -> Bool)
   quickCheck (functorCompose (+1) (*3) :: EvilGoateeConst Bool Int -> Bool)
   quickCheck (functorId :: LiftItOut Maybe Int -> Bool)
   quickCheck (functorCompose (+1) (*2) :: LiftItOut Maybe Int -> Bool)
   quickCheck (functorId :: Parappa Maybe Maybe Int -> Bool)
   quickCheck (functorCompose (+1) (*2) :: Parappa Maybe Maybe Int -> Bool)
   quickCheck (functorId :: IgnoreOne Maybe Maybe Int Int -> Bool)
   quickCheck (functorCompose (+1) (*2) :: IgnoreOne Maybe Maybe Int Int -> Bool)
   quickCheck (functorId :: List Int -> Bool)
   quickCheck (functorCompose (+1) (*2) :: List Int -> Bool)
   quickCheck (functorId :: GoatLord Int -> Bool)
   quickCheck (functorCompose (*2) (*4) :: GoatLord Int -> Bool)