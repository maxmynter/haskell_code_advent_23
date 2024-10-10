import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO (readFile)

type CardNumber = Int

type WinningNumber = Int

type YourNumber = Int

type CardsMap = Map.Map CardNumber ([WinningNumber], [YourNumber])

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

nWins :: [WinningNumber] -> [YourNumber] -> Int
nWins wins yours = length $ filter (`elem` wins) yours

getCardsMap :: [String] -> CardsMap
getCardsMap lines = Map.fromList $ map convert lines
  where
    convert s = let (c, w, y) = parseLine s in (c, (w, y))

countCards :: CardsMap -> Int -> Int
countCards cardsMap maxCard = sum $ Map.elems $ foldl' processCard initialCounts [1 .. maxCard]
  where
    initialCounts = Map.fromList [(i, 1) | i <- [1 .. maxCard]]
    incBy = Map.insertWith (+)
    processCard counts cardNum =
      case Map.lookup cardNum cardsMap of
        Just (w, y) ->
          let nCards = counts Map.! cardNum
              wins = nWins w y
              winRange = [cardNum + 1 .. cardNum + wins]
              addCardsIn mp key = incBy key nCards mp
           in foldl' addCardsIn counts winRange
        Nothing -> counts

main :: IO ()
main = do
  content <- lines <$> readFile "d4/input.txt"
  let cardsMap = getCardsMap content
  let totalCards = countCards cardsMap (length content)
  putStrLn $ "Total cards including copies: " ++ show totalCards
