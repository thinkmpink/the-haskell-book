-- typeKwonDo1.hs

module TypeKwonDo1 where

-- 1.

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
-- h = ???
h = g . f


-- 2.

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
-- e = ???
e = w . q


-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
-- xform = ???
xform (x, y) = (xz x, yz y)


-- 4.
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ = fst . yToWZ . xToY
