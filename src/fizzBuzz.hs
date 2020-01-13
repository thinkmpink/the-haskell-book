import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0  = "FizzBuzz"
           | n `mod` 5 == 0   = "Buzz"
           | n `mod` 3 == 0   = "Fizz"
           | otherwise        = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult i = do
  strs <- get
  put $ fizzBuzz i : strs

main :: IO ()
main =
  mapM_ putStrLn $
    reverse $ fizzbuzzList [1..100]


fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start end =
  execState (mapM_ addResult
    $ enumFromThenTo end (pred end) start) []

testFizzBuzz :: IO ()
testFizzBuzz =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
