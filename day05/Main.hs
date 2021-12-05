import Data.Text (splitOn, pack, unpack, Text)
import Data.List ( group, sort )

type Point = (Int, Int)
type Line = (Point, Point)

parsePoint :: Text -> Point
parsePoint p
  = (read . unpack $ x, read . unpack $ y)
  where
    (x : y : _) = splitOn (pack ",") p

parseLine :: String -> Line
parseLine l
  = (parsePoint from, parsePoint to)
  where
    (from : to : _) = splitOn (pack "->") (pack l)

getInput :: String -> IO [Line]
getInput fileName
  = do
      contents <- readFile fileName
      let ls = lines contents
      return (map parseLine ls)

isHorVer :: Line -> Bool
isHorVer ((x1, y1), (x2, y2))
  = x1 == x2 || y1 == y2

pointsOnLine :: Line -> [Point]
pointsOnLine ((x1, y1), (x2, y2))
  | x1 == x2  = zip (repeat x1) [minY..maxY]
  | y1 == y2  = zip [minX..maxX] (repeat y1)
  | otherwise = zip xs ys
  where
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2
    xs = if x1 <= x2 then [x1..x2] else [x1,x1-1..x2]
    ys = if y1 <= y2 then [y1..y2] else [y1,y1-1..y2]

isCovering :: Point -> Line -> Bool
isCovering (px, py) ((x1, y1), (x2, y2))
  | x1 == x2  = c1
  | otherwise = c2
  where
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2
    m = (y2 - y1) `div` (x2 - x1)
    c1 = x2 == px && minY <= py && maxY >= py
    c2 = py - y1 == m * (px - x1) &&
      minX <= px && maxX >= px &&
      minY <= py && maxY >= py

numCovering :: Point -> [Line] -> Int
numCovering p ls
  = sum $ map (\l -> if isCovering p l then 1 else 0) ls

partOne :: [Line] -> [Point] -> Int
partOne [] ps 
  = length $ map head $ group $ sort ps 
partOne (l : ls) ps
  = partOne ls newPs
  where
    lps = pointsOnLine l
    newPs = ps ++ filter (\p -> numCovering p ls >= 1) lps

partTwo :: [Line] -> [Point] -> Int
partTwo [] ps
  = length $ map head $ group $ sort ps
partTwo (l : ls) ps
  = partTwo ls newPs
  where
    lps = pointsOnLine l
    newPs = ps ++ filter (\p -> numCovering p ls >= 1) lps

main :: IO ()
main
  = do
      ls <- getInput "input.txt"
      print $ partOne (filter isHorVer ls) []
      print $ partTwo ls []