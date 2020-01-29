Chapter Exercises

> module MonadTransChapterExercises where
>
> import Control.Monad
> import Control.Monad.IO.Class
> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Identity
> import Control.Monad.Trans.Maybe
> import Control.Monad.Trans.Reader
> import Control.Monad.Trans.State.Lazy

Write the code

1. rDec is a function that should get its argument in the context of Reader and return a value decremented by one:

> rDec :: Num a => Reader a a
> rDec = (+ (-1)) <$> ask

2. Once you have an rDec that works, make it and any inner lambdas point-free, if that's not already the case.

done.

3. rShow is show, but in Reader:

> newtype Identity a =
>   Identity { runIdentity :: a }

> instance Show a
>       => Show (Identity a) where
>   show (Identity a) = show a

> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)

> instance Applicative Identity where
>   pure = Identity
>   Identity f <*> Identity a =
>     Identity $ f a

> instance Monad Identity where
>   Identity a >>= f = f a

> rShow :: Show a
>       => ReaderT a Identity String
> rShow = show <$> ask

4. Once you have an rShow that works, make it point-free.

done.

5. rPrintAndInc will first print the input with a greeting, then return the input incremented by one:

> rPrintAndInc :: (Num a, Show a)
>              => ReaderT a IO a
> rPrintAndInc = do
>   a <- ask
>   liftIO . putStrLn $ "Hi: " <> show a
>   return $ a + 1

6. sPrintIncAccum first prints the input with a greeting, then "puts" the incremented input as the new state and returns the original input as a String:

> sPrintIncAccum :: (Num a, Show a)
>                => StateT a IO String
> sPrintIncAccum = do
>   i <- get
>   liftIO . putStrLn $ "Hi: " <> show i
>   put $ i + 1
>   return $ show i

7. Fix the code

The code won't type check as written; fix it so that it does. Feel free to add imports if they provide something useful. Functions are used that we haven't introduced. You're not allowed to change the types asserted. You may have to fix the code in more than one place.

> isValid :: String -> Bool
> isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- getLine
  guard $ isValid v
  return v

> maybeExcite :: MaybeT IO String
> maybeExcite = do
>   v <- liftIO getLine
>   guard $ isValid v
>   return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- maybeExcite

  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
        ("Good, was very excite: " ++ e)

> doExcite :: IO ()
> doExcite = do
>   putStrLn "say something excite!"
>   excite <- runMaybeT maybeExcite
>
>   case excite of
>     Nothing -> putStrLn "MOAR EXCITE"
>     Just e ->
>       putStrLn
>         ("Good, was very excite: " ++ e)

8. Hit counter

See hitCounter.hs
