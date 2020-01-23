-- Problem 55

data Tree a =
    Empty
  | Node a (Tree a) (Tree a)

infTree :: Tree Char
infTree = Node 'x' infTree infTree

cBal :: Integer -> [Tree Char]
cBal 1 = Node 'x' Empty Empty
cBal 2 = Node 'x' (cBal 1) Empty
cBal n = undefined
