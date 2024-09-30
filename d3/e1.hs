import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (mapMaybe)

type Matrix a = [[a]]

readMtxFromFile :: FilePath -> IO (Matrix Char)
readMtxFromFile path = do
  content <- readFile path
  return $ lines content

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

adjacents :: Int -> Int -> Int -> [(Int, Int)]
adjacents x y1 y2 =
  [(x - 1, y) | y <- [y1 - 1 .. y2 + 1]]
    ++ [(x, y1 - 1), (x, y2 + 1)]
    ++ [(x + 1, y) | y <- [y1 - 1 .. y2 + 1]]

inBounds :: Matrix a -> (Int, Int) -> Bool
inBounds mtx (x, y) =
  x >= 0 && x < length mtx && y >= 0 && y < length (head mtx)

hasAdjacentSymbol :: Matrix Char -> (Int, Int) -> Int -> Bool
hasAdjacentSymbol mtx (x, y) len =
  any (\(dx, dy) -> inBounds mtx (dx, dy) && isSymbol (mtx !! dx !! dy)) (adjacents x y (y + len - 1))

extractNumber :: Matrix Char -> (Int, Int) -> (Maybe Int, Int)
extractNumber mtx (x, y)
  | not (inBounds mtx (x, y)) || not (isDigit (mtx !! x !! y)) = (Nothing, 0)
  | otherwise =
      let numStr = takeWhile isDigit $ drop y (mtx !! x)
       in (Just $ read numStr, length numStr)

processMatrix :: Matrix Char -> [Int]
processMatrix mtx =
  [ num
    | x <- [0 .. length mtx - 1],
      y <- [0 .. length (head mtx) - 1],
      isDigit (mtx !! x !! y),
      y == 0 || not (isDigit (mtx !! x !! (y - 1))),
      let (maybeNum, len) = extractNumber mtx (x, y),
      hasAdjacentSymbol mtx (x, y) len,
      Just num <- [maybeNum]
  ]

main :: IO ()
main = do
  mtx <- readMtxFromFile "d3/input.txt"
  let nums = processMatrix mtx
  print $ sum nums
