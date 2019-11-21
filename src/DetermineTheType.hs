{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where
-- 1.
-- oneA :: Num a => a
oneA = (* 9) 6

-- oneB :: Num a => (a, [Char])
oneB = head [(0, "doge"),(1, "kitteh")]

-- oneC :: (Integer, [Char])
oneC = head [(0 :: Integer , "doge"),(1, "kitteh")]

-- oneD :: Bool
oneD = if False then True else False

-- oneE :: Int
oneE = length [1, 2, 3, 4, 5]

-- oneF :: Bool
oneF = (length [1, 2, 3, 4, 5]) > (length "TACOCAT")


-- 2. Given
two =
  let x = 5
      y = x + 5
      w = y * 10
  in w
-- What is the type of w?
-- w :: Num a => a

three = let x = 5
            y = x + 5
            z y = y * 10
        in z
-- What is the type of z?
-- z :: Num a => a -> a

four = let x = 5
           y = x + 5
           f = 4 / y
       in f
-- What is the type of f?
-- f :: Fractional a => a

five = let x = "Julie"
           y = " <3 "
           z = "Haskell"
           f = x ++ y ++ z
       in f

-- What is the type of f?
-- f :: [Char]
