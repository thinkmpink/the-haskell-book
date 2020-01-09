import Control.Applicative
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

-- The output of those two should be identical:
-- one string that is made all uppercase and reversed,
-- like this:
-- Prelude> composed "Julie"
-- "EILUJ"
-- Prelude> fmapped "Chris"
-- "SIRHC"


-- Now we want to return the results of
-- cap and rev both, as a tuple, like this:
--  Prelude> tupled "Julie"
-- ("JULIE","eiluJ")
-- -- or
--  Prelude> tupled' "Julie"
-- ("eiluJ","JULIE")
-- We will want to use an Applicative here.
-- The type will look like this:
-- tupled :: [Char] -> ([Char], [Char])

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) rev cap

-- There is no special reason such a function
-- needs to be monadic, but let’s do that, too,
-- to get some practice. Do it one time using
-- do syntax; then try writing a new version
-- using (>>=). The types will be the same as
-- the type for tupled.

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  c <- cap
  r <- rev
  return (c, r)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' =
  cap >>= \c ->
  rev >>= \r ->
  return (c, r)
