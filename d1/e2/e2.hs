import Data.Char (isDigit)
import Data.List (find, isPrefixOf)
import System.IO

spelledToDigit :: String -> String
spelledToDigit = go
  where
    spelledDigits = [("zero", '0'), ("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]
    go [] = []
    go (c : cs)
      | isDigit c = c : go cs
      | otherwise = case find (\(word, _) -> word `isPrefixOf` (c : cs)) spelledDigits of
          Just (word, digit) -> digit : go cs
          Nothing -> go cs

concatFirstLastNumber :: String -> String
concatFirstLastNumber cs = [findFirst cs, findLast cs]
  where
    findFirst = head . filter isDigit
    findLast = findFirst . reverse

main :: IO ()
main = do
  content <- readFile "input.txt"
  let result = sum $ map (read . concatFirstLastNumber . spelledToDigit) (lines content)
  print result
