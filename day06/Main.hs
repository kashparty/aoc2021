import Data.Text (splitOn, pack, unpack)
import Data.List

getInput :: String -> IO [Int]
getInput fileName
  = do
      contents <- readFile fileName
      return $ map (read . unpack) $ splitOn (pack ",") (pack contents)

-- Bad way
partOne :: [Int] -> Int -> Int
partOne nums 0
  = length nums
partOne nums days
  = partOne newNums (days - 1)
  where
    zeroCount = length $ filter (== 0) nums
    nextDayNums = [nextDay i | i <- nums]
    newNums = nextDayNums ++ replicate zeroCount 8

    nextDay :: Int -> Int
    nextDay n
      | n == 0    = 6
      | otherwise = n - 1

-- Good way
partTwo :: [Int] -> Int -> Int
partTwo nums = partTwo' sizes
  where
    sizes = [length $ filter (== i) nums | i <- [0..8]]

    partTwo' :: [Int] -> Int -> Int
    partTwo' sizes 0
      = sum sizes
    partTwo' (zs : rest) days
      = partTwo' newSizes (days - 1)
      where
        newSizes = 
          [if i == 6 then (rest !! i) + zs else rest !! i | i <- [0..7]] ++ [zs]


main :: IO ()
main
  = do
      nums <- getInput "input.txt"
      print $ partOne nums 80
      print $ partTwo nums 256