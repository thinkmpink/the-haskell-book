-- functors3.hs

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap f FixMe = FixMe
  fmap f (Pls a) = Pls (f a)
