module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

one''  = one >> eof >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two character,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

oneTwo'' = oneTwo >> eof >> stop

p123 :: String -> Result Int
p123 s = read <$> parseString (string s) mempty s

p123' :: String -> Result Int
p123' s = read
  <$> parseString (string s >> stop) mempty s

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

-- Try writing a Parser that does what string does,
-- but using char.
string' :: CharParsing m => String -> m String
string' = traverse char

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "one'':"
  testParse one''

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

  pNL "oneTwo'':"
  testParse oneTwo''
