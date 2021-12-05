import Data.List
import Data.Text (splitOn, pack, unpack)

type Board = [[(Int, Bool)]]

setUpBoard :: [[Int]] -> Board
setUpBoard = map (map (\x -> (x, False)))

getInput :: String -> IO ([Int], [Board])
getInput fileName
  = do
      contents <- readFile fileName
      let (firstLine : boardStrings) = splitOn (pack "\n\n") (pack contents)
      let nums = map (read . unpack) $ splitOn (pack ",") firstLine
      let boards = map (map (map read . words) . lines . unpack) boardStrings
      return (nums, map setUpBoard boards)

getCols :: Board -> [[(Int, Bool)]]
getCols board
  = [[row !! n | row <- board] | n <- [0..length (head board) - 1]]

checkWinning :: Board -> Bool
checkWinning board
  = or [all snd row | row <- board] || or [all snd col | col <- getCols board]

updateBoard :: Int -> Board -> Board
updateBoard n board
  = [[if x == n then (x, True) else (x, m) | (x, m) <- row] | row <- board]

score :: Board -> Int -> Int
score board n
  = n * sumUnmarked
  where
    sumUnmarked = sum [sum [if m then 0 else x | (x, m) <- row] | row <- board]

partOne :: [Int] -> [Board] -> Int
partOne [] _
  = 0
partOne (n : ns) boards
  = case winningBoard of
      Just board -> score board n
      Nothing -> partOne ns newBoards
  where
    newBoards = map (updateBoard n) boards
    winningBoard = find checkWinning newBoards

partTwo :: [Int] -> [Board] -> Int 
partTwo [] _
  = 0
partTwo (n : ns) boards
  = case (boards, newBoards) of
    ([b], []) -> score (updateBoard n b) n
    (_, bs) -> partTwo ns bs
  where
    newBoards = filter (not . checkWinning) $ map (updateBoard n) boards

main :: IO ()
main
  = do
      (nums, boards) <- getInput "input.txt"
      print $ partOne nums boards
      print $ partTwo nums boards