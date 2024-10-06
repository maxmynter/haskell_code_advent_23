import Data.Char (isDigit)
import Data.List (nub)
import Data.Maybe (catMaybes)

type MatrixRow a = [a]

type Matrix a = [MatrixRow a]

type Number = Int

type Start = Int

type Coord = (Int, Int)

readMtxFromFile :: FilePath -> IO (Matrix Char)
readMtxFromFile = fmap lines . readFile

extractNumber :: Matrix Char -> Int -> Int -> Maybe (Number, Start)
extractNumber mtx row probe
  | not (inBounds mtx row probe) || not (isDigit (mtx !! row !! probe)) = Nothing
  | otherwise =
      let (before, after) = splitAt probe (mtx !! row)
          beforeDigits = takeWhile isDigit (reverse before)
          afterDigits = takeWhile isDigit after
          numStr = reverse beforeDigits ++ afterDigits
       in if null numStr
            then Nothing
            else Just (read numStr, probe - length beforeDigits)

inBounds :: Matrix a -> Int -> Int -> Bool
inBounds mtx row col = row >= 0 && row < length mtx && col >= 0 && col < length (head mtx)

isGear :: Char -> Bool
isGear = (==) '*'

getAdjacentCoords :: Coord -> [Coord]
getAdjacentCoords (x, y) =
  [ (x - 1, y - 1),
    (x - 1, y),
    (x - 1, y + 1),
    (x, y - 1),
    (x, y + 1),
    (x + 1, y - 1),
    (x + 1, y),
    (x + 1, y + 1)
  ]

getAdjacentNumbers :: Matrix Char -> Coord -> [Number]
getAdjacentNumbers mtx coord =
  nub . map fst . catMaybes $ [extractNumber mtx r c | (r, c) <- getAdjacentCoords coord]

gearRatio :: Matrix Char -> Coord -> Int
gearRatio mtx coord =
  let adjacents = getAdjacentNumbers mtx coord
   in case adjacents of
        [n1, n2] -> n1 * n2
        _ -> 0

findGears :: Matrix Char -> [Coord]
findGears mtx =
  [ (x, y)
    | (x, row) <- zip [0 ..] mtx,
      (y, char) <- zip [0 ..] row,
      isGear char
  ]

sumGearRatios :: Matrix Char -> Int
sumGearRatios mtx = sum $ map (gearRatio mtx) (findGears mtx)

main :: IO ()
main = do
  mtx <- readMtxFromFile "d3/input.txt"
  let result = sumGearRatios mtx
  print result
