-- functors2.hs

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap =
    error
    "won't compile."
