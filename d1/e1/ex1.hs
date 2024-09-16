import Control.Monad
import Data.Char (isDigit)
import System.IO
import Text.Read (readMaybe)

concatFirstLastNumber :: String -> String
concatFirstLastNumber cs = [findFirst cs, findLast cs]
  where
    findFirst = head . filter isDigit
    findLast = findFirst . reverse

main :: IO ()
main = do
  content <- readFile "input.txt"
  let result = sum $ map (read . concatFirstLastNumber) (lines content)
  print result
