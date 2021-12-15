import Data.List hiding (insert, delete)
import Data.Map (Map, fromList, member, insert, toList, delete, (!))

getInput :: String -> IO [[Int]]
getInput fileName
  = do
      contents <- readFile fileName
      return $ map (map (\c -> read [c])) $ lines contents

at :: [[Int]] -> (Int, Int) -> Int
at grid (r, c)
  = grid !! r !! c

dijkstra :: [[Int]] -> Map (Int, Int) Int -> (Int, Int) -> Int -> Int
dijkstra grid unvisited o@(or, oc) d 
  | arrived   = d 
  | otherwise = dijkstra grid finalUnvisited newOrigin newDist
  where
    arrived = or == length grid - 1 && oc == length (head grid) - 1
    up = (or - 1, oc)
    down = (or + 1, oc)
    left = (or, oc - 1)
    right = (or, oc + 1)
    newUnvisited = foldl (\m u -> if member u m 
                                  then insert u (min (m ! u) (d + at grid u)) m 
                                  else m) unvisited [up, down, left, right]
    ((newOrigin, newDist) : _) = sortOn snd $ toList newUnvisited
    finalUnvisited = delete newOrigin newUnvisited

partOne :: [[Int]] -> Int
partOne grid
  = dijkstra grid (fromList (concat [[((r, c), 1000000000) 
                            | c <- [0..length (head grid) - 1]] 
                            | r <- [0..length grid - 1]])) (0, 0) 0

-- This takes a couple hours to run it's really bad
partTwo :: [[Int]] -> Int
partTwo grid
  = dijkstra bigGrid (fromList (concat [[((r, c), 1000000000)
                            | c <- [0..length (head grid) * 5 - 1]]
                            | r <- [0..length grid * 5 - 1]])) (0, 0) 0
  where
    bigGrid = [[(grid !! (r `mod` length grid) !! (c `mod` length (head grid))
                + (r `div` length grid) + (c `div` length (head grid)) - 1) 
                `mod` 9 + 1
                | c <- [0..length (head grid) * 5 - 1]] 
                | r <- [0..length grid * 5 - 1]]

main :: IO ()
main
  = do
      grid <- getInput "input.txt"
      print $ partOne grid
      print $ partTwo grid