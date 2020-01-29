Chapter 26. Monad Transformers

> {-# LANGUAGE InstanceSigs #-}
>
> module MonadTransformers where
>
> import Control.Monad
> import Control.Monad.IO.Class
> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Except
> import Control.Monad.Trans.Reader

> newtype MaybeT m a =
>   MaybeT { runMaybeT :: m (Maybe a) }

> instance (Functor m)
>       => Functor (MaybeT m) where
>   fmap f (MaybeT ma) =
>     MaybeT $ (fmap . fmap) f ma

> instance (Applicative m)
>       => Applicative (MaybeT m) where
>   pure = MaybeT . pure . pure
>
>   MaybeT fab <*> MaybeT mma =
>     MaybeT ((<*>) <$> fab <*> mma)

> instance (Monad m)
>       => Monad (MaybeT m) where
>   (>>=) :: MaybeT m a
>         -> (a -> MaybeT m b)
>         -> MaybeT m b
>   MaybeT mma >>= f =
>     MaybeT $ do
>       ma <- mma
>       case ma of
>         Nothing -> return Nothing
>         Just a -> runMaybeT $ f a

Exercises: EitherT

> newtype EitherT e m a =
>   EitherT { runEitherT :: m (Either e a) }

1. Write the Functor instance for EitherT:

> instance Functor m
>       => Functor (EitherT e m) where
>   fmap f (EitherT m) =
>     EitherT $ (fmap . fmap) f m

2. Write the Applicative instance for EitherT

> instance Applicative m
>       => Applicative (EitherT e m) where
>   pure = EitherT . pure . pure
>
>   EitherT meab <*> EitherT mea =
>     EitherT $ ((<*>) <$> meab <*> mea)

3. Write the Monad instance for EitherT:

> instance Monad m
>       => Monad (EitherT e m) where
>   (>>=) :: EitherT e m a
>         -> (a -> EitherT e m b)
>         -> EitherT e m b
>   EitherT mea >>= f =
>     EitherT $ do
>       ea <- mea
>       case ea of
>         Left e -> return $ Left e
>         Right a -> runEitherT $ f a

4. Write the swapEitherT helper function for EitherT.

> swapEitherT :: Functor m
>             => EitherT e m a
>             -> EitherT a m e
> swapEitherT (EitherT mea) =
>   EitherT $ swapEither <$> mea

> swapEither :: Either e a -> Either a e
> swapEither (Left e) = Right e
> swapEither (Right a) = Left a

5. Write the transformer variant of the either catamorphism.

> eitherT :: Monad m =>
>            (a -> m c)
>         -> (b -> m c)
>         -> EitherT a m b
>         -> m c
> eitherT amc bmc (EitherT mab) = do
>     ab <- mab
>     case ab of
>       Left a -> amc a
>       Right b -> bmc b

ReaderT

> newtype ReaderT r m a =
>   ReaderT { runReaderT :: r -> m a }

> instance Functor m
>       => Functor (ReaderT r m) where
>   fmap :: (a -> b)
>        -> ReaderT r m a
>        -> ReaderT r m b
>   fmap f (ReaderT rma) =
>     ReaderT $ (fmap . fmap) f rma

> instance Applicative m
>       => Applicative (ReaderT r m) where
>   pure = ReaderT . pure . pure
>   (<*>) :: ReaderT r m (a -> b)
>         -> ReaderT r m a
>         -> ReaderT r m b
>   ReaderT rmab <*> ReaderT rma =
>     ReaderT $ ((<*>) <$> rmab <*> rma)

> instance Monad m
>       => Monad (ReaderT r m) where
>   (>>=) :: ReaderT r m a
>         -> (a -> ReaderT r m b)
>         -> ReaderT r m b
>   ReaderT rma >>= f =
>     ReaderT $ \r -> do
>       a <- rma r
>       runReaderT (f a) r

StateT

> newtype StateT s m a =
>   StateT { runStateT :: s -> m (a, s) }

Exercises: StateT

1. Functor

> instance (Functor m)
>       => Functor (StateT s m) where
>   fmap f (StateT smas) =
>     StateT $ (fmap . fmap)
>              (\(a, s) -> (f a, s)) smas

> instance Monad m
>       => Applicative (StateT s m) where
>   pure a = StateT $ \s -> pure (a, s)
>
>   StateT smabs <*> StateT smas =
>     StateT $ \s1 -> do
>       (ab, s2) <- smabs s1
>       (a, s3)  <- smas s2
>       return (ab a, s3)

> instance Monad m
>       => Monad (StateT s m) where
>   (StateT smas) >>= f =
>     StateT $ \s1 -> do
>       (a, s2) <- smas s1
>       runStateT (f a) s2

Exercise: Wrap It Up
Turn readerUnwrap from the previous example back into embedded through the use of the data constructors for each transformer.

> embedded :: MaybeT
>             (ExceptT String
>                      (ReaderT () IO))
>             Int
> embedded = (MaybeT . ExceptT . ReaderT . fmap pure)
>              (const (Right (Just 1)))

Exercises: Lift more

> instance MonadTrans MaybeT where
>   lift = MaybeT . liftM Just

> instance MonadTrans (ReaderT r) where
>   lift = ReaderT . const

1. MonadTrans instance for EitherT

> instance MonadTrans (EitherT e) where
>   lift = EitherT . liftM Right

2. Same for StateT

> instance MonadTrans (StateT s) where
>   lift :: Monad m => m a -> StateT s m a
>   lift = StateT . \ma s -> do
>                    a <- ma
>                    return (a, s)

MonadIO

Exercises: Some instances

1. MaybeT

> instance (MonadIO m)
>       => MonadIO (MaybeT m) where
>   liftIO = lift . liftIO

2. ReaderT

> instance (MonadIO m)
>       => MonadIO (ReaderT r m) where
>   liftIO = lift . liftIO

3. StateT

> instance (MonadIO m)
>       => MonadIO (StateT s m) where
>   liftIO = lift . liftIO
