module Day2 where

import Data.List.Split (splitOn)

type ID = Int

type Red = Int

type Blue = Int

type Green = Int

data Round = Round Red Blue Green

data Game = Game ID [Round]

extractMaxOccurrences :: Game -> (Red, Blue, Green)
extractMaxOccurrences (Game _ rounds) = go (0, 0, 0) rounds
  where
    go rgbs [] = rgbs
    go (maxR, maxG, maxB) ((Round r g b) : rs) = go (max r maxR, max maxG g, max maxB b) rs

power :: Game -> Int
power gm = let (r, g, b) = extractMaxOccurrences gm in r * g * b

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
  content <- readFile "d2/input.txt"
  let result = sum [power (parseGame l) | l <- lines content]
  print result
