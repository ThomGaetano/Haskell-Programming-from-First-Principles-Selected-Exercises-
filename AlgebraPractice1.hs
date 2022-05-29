--algebraPractice1.hs

module AlgebraPractice1 where

data X a b c = X a (b c) deriving (Eq, Show)

newtype ReaderM r m a = ReaderM { runReaderM :: r -> m a }

newtype StateM s m a = StateM { runStateM :: s -> m (a, s) }

instance Functor b => Functor (X a b) where
   fmap f (X a bc) = X a (fmap f (bc))

instance (Monoid a, Applicative b) => Applicative (X a b) where
   pure x = X mempty (pure x)
   (<*>) (X a (bf)) (X a' (bx)) = X (a <> a') (bf <*> bx)

instance Foldable b => Foldable (X a b) where
   foldMap f (X a (bc)) = foldMap f (bc)

instance Traversable b => Traversable (X a b) where
   traverse f (X a (bc)) = sequenceA (X a (f <$> (bc)))

instance Functor m => Functor (ReaderM r m) where
   fmap f (ReaderM g) = ReaderM $ (fmap . fmap) f g

instance Applicative m => Applicative (ReaderM r m) where
   pure x = ReaderM $ \r -> (pure x)
   (<*>) (ReaderM rmf) (ReaderM rma) = ReaderM $ \r -> (rmf r) <*> (rma r)

instance Monad m => Monad (ReaderM r m) where
   return = pure
   (>>=) (ReaderM rma) f = ReaderM $ \r -> (rma r) >>= fmap ($r) (runReaderM . f)

instance Functor m => Functor (StateM s m) where
   fmap f (StateM smas) = StateM $ \s -> fmap (($s) . (,) . f . fst) (smas s)

instance Applicative m => Applicative (StateM s m) where
   pure x = StateM $ \s -> pure (x, s)
   (<*>) (StateM smfs) (StateM smas) = StateM $ \s -> fmap ($s) (fmap (,) ((fmap fst (smfs s)) <*> (fmap fst (smas s))))

instance Monad m => Monad (StateM s m) where
   return = pure
   (>>=) (StateM smas) f = StateM $ \s -> (smas s) >>= fmap ($s) (runStateM . f . fst)