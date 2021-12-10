import Data.Char
import Data.List

getInput :: String -> IO [[Int]]
getInput fileName
  = do
      contents <- readFile fileName
      return $ map (map (\c -> ord c - ord '0')) $ lines contents

heightAt :: [[Int]] -> Int -> Int -> Int
heightAt hs r c
  = hs !! r !! c

isLow :: [[Int]] -> Int -> Int -> Bool
isLow hs r c
  = top && bottom && left && right
  where
    this = heightAt hs r c
    top = r == 0 || heightAt hs (r - 1) c > this
    bottom = r == length hs - 1 || heightAt hs (r + 1) c > this
    left = c == 0 || heightAt hs r (c - 1) > this
    right = c == length (head hs) - 1 || heightAt hs r (c + 1) > this

partOne :: [[Int]] -> Int
partOne hs
  = sum $ concat lows
  where
    lows = [[1 + heightAt hs r c | c <- [0..length (head hs) - 1], isLow hs r c]
                          | r <- [0..length hs - 1]]

partTwo :: [[Int]] -> Int
partTwo hs
  = head prods
  where
    lows = concat [[(r, c) | c <- [0..length (head hs) - 1], isLow hs r c]
                    | r <- [0..length hs - 1]]
    
    sizes = sortBy (flip compare) $ map (\p -> basinBFS [p] []) lows
    prods = zipWith3 (\a b c -> a * b * c) sizes (tail sizes) (drop 2 sizes)

    basinBFS :: [(Int, Int)] -> [(Int, Int)] -> Int
    basinBFS [] seen
      = length seen
    basinBFS ((r, c) : rest) seen
      | heightAt hs r c == 9 = basinBFS rest seen
      | (r, c) `elem` seen   = basinBFS rest seen
      | otherwise            = basinBFS newQueue ((r, c) : seen)
      where
        topQueue = [(r - 1, c) | r > 0]
        bottomQueue = [(r + 1, c) | r < length hs - 1]
        leftQueue = [(r, c - 1) | c > 0]
        rightQueue = [(r, c + 1) | c < length (head hs) - 1]
        newQueue = rest ++ concat [topQueue, bottomQueue, leftQueue, rightQueue]

main :: IO ()
main
  = do
      hs <- getInput "input.txt"
      print $ partOne hs
      print $ partTwo hs