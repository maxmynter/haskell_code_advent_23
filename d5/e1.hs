import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

type Range = (Int, Int, Int)

parseInput :: String -> ([Int], [[Range]])
parseInput input = (seeds, maps)
  where
    (seedsStr : mapsStr) = splitOn "\n\n" input
    seeds = map read . words . drop 7 $ seedsStr
    maps = map parseMap mapsStr

parseMap :: String -> [Range]
parseMap = map parseRange . drop 1 . lines
  where
    parseRange line =
      let [dest, src, len] = map read $ words line
       in (dest, src, len)

applyMap :: [Range] -> Int -> Int
applyMap ranges n = fromMaybe n $ do
  (dest, src, len) <- find (\(_, src, len) -> n >= src && n < src + len) ranges
  return $ dest + (n - src)

processNumber :: [[Range]] -> Int -> Int
processNumber maps n = foldl (flip applyMap) n maps

solve :: ([Int], [[Range]]) -> Int
solve (seeds, maps) = minimum $ map (processNumber maps) seeds

main :: IO ()
main = do
  input <- readFile "d5/input.txt"
  print $ solve $ parseInput input
