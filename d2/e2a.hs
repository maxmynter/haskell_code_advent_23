import Data.List.Split (splitOn)

type ID = Int

type Red = Int

type Blue = Int

type Green = Int

data Round = Round Red Blue Green

data Game = Game ID [Round]

isRoundValid :: Red -> Blue -> Green -> Round -> Bool
isRoundValid maxR maxG maxB rnd@(Round r g b) =
  r <= maxR && g <= maxG && b <= maxB

allRoundsValid :: Red -> Blue -> Green -> [Round] -> Bool
allRoundsValid maxR maxG maxB rounds =
  and [isRoundValid maxR maxG maxB round | round <- rounds]

idIfValidElseZero :: Red -> Green -> Blue -> Game -> Int
idIfValidElseZero maxR maxG maxB game@(Game id rounds) =
  if allRoundsValid maxR maxG maxB rounds then id else 0

parseColorStr :: String -> Round
parseColorStr s = foldl go (Round 0 0 0) (splitOn "," s)
  where
    go (Round r g b) colorStr =
      case words colorStr of
        [numStr, color] ->
          let num = read numStr
           in case color of
                "red" -> Round (max r num) g b
                "green" -> Round r (max g num) b
                "blue" -> Round r g (max b num)
        _ -> Round r g b

parseGame :: String -> Game
parseGame s =
  let [gameStr, roundsStr] = splitOn ":" s
      gameId = read $ last $ words gameStr
      rounds = map parseColorStr $ splitOn ";" roundsStr
   in Game gameId rounds

main :: IO ()
main = do
  content <- readFile "input.txt"
  let result = sum [idIfValidElseZero 12 13 14 (parseGame l) | l <- (lines content)]
  print $ result
