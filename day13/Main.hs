import Data.Text (pack, unpack, splitOn, Text)
import Data.List

type Point = (Int, Int)
data Line = Horizontal Int | Vertical Int

parsePoint :: String -> Point
parsePoint s
  = read $ "(" ++ s ++ ")"

parseLine :: String -> Line
parseLine s
  | horizontal = Horizontal value 
  | otherwise  = Vertical value 
  where
    horizontal = 'y' `elem` s
    (_ : valueString : _) = splitOn (pack "=") (pack s)
    value = read $ unpack valueString

getInput :: String -> IO ([Point], [Line])
getInput fileName
  = do
      contents <- readFile fileName
      let (ps : ls : _) = splitOn (pack "\n\n") (pack contents)
      let pointList = map parsePoint (lines $ unpack ps)
      let lineList = map parseLine (lines $ unpack ls)
      return (pointList, lineList)

reflectPoint :: Line -> Point -> Point
reflectPoint (Horizontal v) (px, py)
  | py >= v   = (px, 2 * v - py)
  | otherwise = (px, py)
reflectPoint (Vertical v) (px, py)
  | px >= v   = (2 * v - px, py)
  | otherwise = (px, py)

partOne :: [Point] -> [Line] -> Int
partOne ps (l : _) 
  = length unique 
  where
    reflectedPs = map (reflectPoint l) ps
    unique = map head . group $ sort reflectedPs

toString :: [Point] -> String
toString ps
  = concat [[pointToChar (x, y) | x <- [0..maxX]] ++ "\n" | y <- [0..maxY]]
  where
    maxX = maximum (map fst ps)
    maxY = maximum (map snd ps)

    pointToChar :: Point -> Char
    pointToChar p
      | p `elem` ps = '#'
      | otherwise   = ' '

partTwo :: [Point] -> [Line] -> String 
partTwo ps []
  = toString ps
partTwo ps (l : ls)
  = partTwo unique ls
  where
    reflectedPs = map (reflectPoint l) ps
    unique = map head . group $ sort reflectedPs

main :: IO ()
main
  = do
      (ps, ls) <- getInput "input.txt"
      print $ partOne ps ls
      putStr $ partTwo ps ls