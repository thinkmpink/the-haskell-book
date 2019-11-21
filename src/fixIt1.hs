module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) -- switch song with > 
         then fstString x
         else sndString y
     where x = "Singin"
           y = "Somewhere"
