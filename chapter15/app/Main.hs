module Main where

import Data.Monoid
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

data Or a b = Fst a | Snd b deriving (Eq, Show)

newtype Combine a b = Combine { unCombine :: (a -> b) }

newtype Comp a = Comp { unComp :: (a -> a) }

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Arbitrary Trivial where
   arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = idGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
   arbitrary = twoGen

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
   arbitrary = threeGen

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
   arbitrary = fourGen

instance Arbitrary BoolConj where
   arbitrary = boolGen

instance Arbitrary BoolDisj where
   arbitrary = boolGen'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
   arbitrary = orGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
   arbitrary = valGen

instance Semigroup Trivial where
   (<>) Trivial Trivial = Trivial

instance Semigroup a => Semigroup (Identity a) where
   (<>) (Identity x) (Identity y) = Identity (x <> y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
   (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
   (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
   (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance Semigroup BoolConj where
   (<>) (BoolConj x) (BoolConj y) = BoolConj (x && y)

instance Semigroup BoolDisj where
   (<>) (BoolDisj x) (BoolDisj y) = BoolDisj (x || y)

instance Semigroup (Or a b) where
   (<>) (Fst x) (Fst y) = Fst y
   (<>) (Snd x) _ = Snd x
   (<>) (Fst x) (Snd y) = Snd y

instance Semigroup b => Semigroup (Combine a b) where
   (<>) (Combine x) (Combine y) = Combine (x <> y)

instance Semigroup (Comp a) where
   (<>) (Comp a) (Comp b) = Comp (a . b)

instance Semigroup a => Semigroup (Validation a b) where
   (<>) (Failure' x) (Failure' y) = Failure' (x <> y)
   (<>) (Success' x) _ = Success' x
   (<>) (Failure' x) (Success' y) = Success' y

instance (Semigroup a, Semigroup s) => Semigroup (Mem s a) where
   (<>) (Mem x) (Mem y) = Mem (x <> y)

instance Monoid Trivial where
   mempty = Trivial
   mappend = (<>)

instance Monoid a => Monoid (Identity a) where
   mempty = Identity (mempty)
   mappend = (<>)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
   mempty = Two (mempty) (mempty)
   mappend = (<>)

instance Monoid BoolConj where
   mempty = BoolConj True
   mappend = (<>)

instance Monoid BoolDisj where
   mempty = BoolDisj False
   mappend = (<>)

instance Monoid b => Monoid (Combine a b) where
   mempty = Combine (mempty)
   mappend = (<>)

instance Monoid a => Monoid (Comp a) where
   mempty = Comp (mempty)
   mappend = (<>)

instance (Monoid a, Monoid s) => Monoid (Mem s a) where
   mempty = Mem (mempty)
   mappend = (<>)

idGen :: Arbitrary a => Gen (Identity a)
idGen = do
   a <- arbitrary
   return (Identity a)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
   a <- arbitrary
   b <- arbitrary
   return (Two a b)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
   a <- arbitrary
   b <- arbitrary
   c <- arbitrary
   return (Three a b c)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
   a <- arbitrary
   b <- arbitrary
   c <- arbitrary
   d <- arbitrary
   return (Four a b c d)

boolGen :: Gen (BoolConj)
boolGen = do
   a <- (arbitrary :: Gen Bool)
   return (BoolConj a)

boolGen' :: Gen (BoolDisj)
boolGen' = do
   a <- (arbitrary :: Gen Bool)
   return (BoolDisj a)

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
   a <- arbitrary
   b <- arbitrary
   elements [Fst a, Snd b]

valGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
valGen = do
   a <- arbitrary
   b <- arbitrary
   elements [Failure' a, Success' b]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId y = mappend mempty y == y

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId y = mappend y mempty == y

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdAssoc = Identity (Sum Integer) -> Identity (Sum Integer) -> Identity (Sum Integer) -> Bool

type TwoAssoc = Two All (Sum Integer) -> Two All (Sum Integer) -> Two All (Sum Integer) -> Bool

type ThreeAssoc = Three All (First Int) (Sum Integer) -> Three All (First Int) (Sum Integer) -> Three All (First Int) (Sum Integer) -> Bool

type FourAssoc = Four All Any (First Int) (Product Integer) -> Four All Any (First Int) (Product Integer) -> Four All Any (First Int) (Product Integer) -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = Or (Sum Integer) (Product Integer) -> Or (Sum Integer) (Product Integer) -> Or (Sum Integer) (Product Integer) -> Bool

type CombAssoc = Combine Integer Integer -> Combine Integer Integer -> Combine Integer Integer -> Bool

type ValAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
   quickCheck (semigroupAssoc :: TrivAssoc)
   quickCheck (semigroupAssoc :: IdAssoc)
   quickCheck (semigroupAssoc :: TwoAssoc)
   quickCheck (semigroupAssoc :: ThreeAssoc)
   quickCheck (semigroupAssoc :: FourAssoc)
   quickCheck (semigroupAssoc :: BoolConjAssoc)
   quickCheck (semigroupAssoc :: BoolDisjAssoc)
   quickCheck (semigroupAssoc :: OrAssoc)
   let failure :: String -> Validation String Int
       failure = Failure'
       success :: Int -> Validation String Int
       success = Success'
   print $ success 1 <> failure "blah"
   print $ failure "woot" <> failure "blah"
   print $ success 1 <> success 2
   print $ failure "woot" <> success 2
   quickCheck (semigroupAssoc :: ValAssoc)
   quickCheck (monoidLeftId :: Trivial -> Bool)
   quickCheck (monoidRightId :: Trivial -> Bool)
   quickCheck (monoidLeftId :: Identity (Sum Int) -> Bool)
   quickCheck (monoidRightId :: Identity (Product Double) -> Bool)
   quickCheck (monoidLeftId :: Two (Sum Int) (Product Int) -> Bool)
   quickCheck (monoidRightId :: Two (Sum Int) String -> Bool)
   quickCheck (monoidLeftId :: BoolConj -> Bool)
   quickCheck (monoidRightId :: BoolConj -> Bool)
   quickCheck (monoidLeftId :: BoolDisj -> Bool)
   quickCheck (monoidRightId :: BoolDisj -> Bool)
   let rmzero = runMem mempty 0
       rmleft = runMem (f' <> mempty) 0
       rmright = runMem (mempty <> f') 0
   print $ rmleft
   print $ rmright
   print $ (rmzero :: (String, Sum Integer))
   print $ rmleft == runMem f' (0 :: Sum Integer)
   print $ rmright == runMem f' (0 :: Sum Integer)