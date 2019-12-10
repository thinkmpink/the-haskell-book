import qualified CipherTest
import qualified WordNumberTest
import qualified Testing

main :: IO ()
main = do
  WordNumberTest.main
  Testing.main
  CipherTest.main
