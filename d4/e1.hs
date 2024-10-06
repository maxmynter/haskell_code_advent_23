import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.IO (readFile)

type CardNumber = Int

type WinningNumber = Int

type YourNumber = Int

cardSeparator = ":"

winningYourSep = "|"

numberSep = " "

countWins :: [WinningNumber] -> [YourNumber] -> Int
countWins wins yours = go 0 wins yours
  where
    go acc wins [] = acc
    go acc wins (y : yours) = if y `elem` wins then go (acc + 1) wins yours else go acc wins yours

parseCards :: String -> [Int]
parseCards cs = map read $ filter (not . null) $ splitOn numberSep cs

parseLine :: String -> (CardNumber, [WinningNumber], [YourNumber])
parseLine line = (cardNum, parseCards winning, parseCards yours)
  where
    cardName : cards : _ = splitOn cardSeparator line
    cardNum = read $ last $ splitOn numberSep cardName :: Int
    winning : yours : _ = splitOn winningYourSep cards

winsOfLine :: String -> Int
winsOfLine line =
  let (_, wins, yours) = parseLine line
      winCount = countWins wins yours
   in if winCount == 0 then 0 else 2 ^ (winCount - 1)

main :: IO ()
main = do
  content <- lines <$> readFile "d4/input.txt"
  let points = sum [winsOfLine line | line <- content]
  print points
