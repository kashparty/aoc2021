import Data.Text (splitOn, pack, unpack)
import Data.Char

parseLine :: String -> (String, String)
parseLine line
  = (unpack from, unpack to)
  where
    (from : to : _) = splitOn (pack "-") (pack line)

getInput :: String -> IO [(String, String)]
getInput fileName
  = do
      contents <- readFile fileName
      return $ map parseLine $ lines contents

connectedTo :: [(String, String)] -> String -> [String]
connectedTo [] _
  = []
connectedTo ((from, to) : rest) search
  | from == search = to : connectedTo rest search
  | to == search   = from : connectedTo rest search
  | otherwise      = connectedTo rest search

partOne :: [(String, String)] -> Int
partOne conns
  = dfs conns [] "start"
  where
    dfs :: [(String, String)] -> [String] -> String -> Int
    dfs _ _ "end"
      = 1
    dfs conns visited origin
      = sum [dfs conns nextVisited next | next <- nexts]
      where
        nexts = filter (`notElem` visited) $ connectedTo conns origin
        nextVisited = if all isUpper origin then visited else origin : visited

partTwo ::[(String, String)] -> Int
partTwo conns
  = dfs conns [] False "start"
  where
    dfs :: [(String, String)] -> [String] -> Bool -> String -> Int
    dfs _ _ _ "end"
      = 1
    dfs conns visited usedDouble origin
      | usedDouble = sum [dfs conns nextVisited True next | next <- nexts]
      | otherwise  = sum [dfs conns nextVisited False next | next <- nexts] 
                   + sum [dfs conns nextVisited True next | next <- visNexts]
      where
        possNexts = connectedTo conns origin
        nexts = filter (`notElem` visited) possNexts 
        visNexts = filter (\x -> x `elem` visited && x /= "start") possNexts
        nextVisited = if all isUpper origin then visited else origin : visited

main :: IO ()
main
  = do
      conns <- getInput "input.txt"
      print $ partOne conns
      print $ partTwo conns