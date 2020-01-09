module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer
   -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'

-- Your outputs from those should look like this:
-- *ReaderPractice> x1
-- Just (6,9)
-- *ReaderPractice> x2
-- Nothing
-- *ReaderPractice> x3 3
-- (Just 9,Just 9)

-- summed is uncurry with addition as the first
-- argument.
summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  -- exercises
  print $ and $ sequA 4
  print $ fromMaybe [] (sequA <$> s')
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 1 ys

  -- Rewriting Shawty
  -- Remember the URL shortener? Instead of manually passing the database connection rConn from main to the app function that gen- erates a Scotty app, use ReaderT to make the database connection available. We know you haven’t seen the transformer variant yet and we’ll explain them soon, but you should try to do the transforma- tion mechanically. Use this version of the app: https://github.com/ bitemyapp/shawty- prime/blob/master/app/Main.hs
-- see ShawtyReader.hs
