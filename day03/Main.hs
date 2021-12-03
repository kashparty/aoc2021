import Data.Char
import Data.List

getInput :: String -> IO [[Int]]
getInput fileName
  = do
      contents <- readFile fileName
      let nums = lines contents
      return $ map (map (\x -> ord x - ord '0')) nums

binToInt :: [Int] -> Int
binToInt []
  = 0
binToInt (x : xs)
  = x + 2 * binToInt xs

partOne :: [[Int]] -> Int
partOne nums
  = gammaNum * epsilonNum
  where
    n = length nums
    oneCounts = foldl (zipWith (+)) (replicate n 0) nums
    gamma = [if count >= n `div` 2 then 1 else 0 | count <- oneCounts]
    epsilon = map (1 - ) gamma

    gammaNum = binToInt $ reverse gamma
    epsilonNum = binToInt $ reverse epsilon

partTwo :: [[Int]] -> Int
partTwo nums
  = oxygenNum * co2Num
  where
    (oxygen : _) = filterOxygen nums
    (co2 : _) = filterCO2 nums

    oxygenNum = binToInt $ reverse oxygen
    co2Num = binToInt $ reverse co2

    filterOxygen :: [[Int]] -> [[Int]]
    filterOxygen ([] : _)
      = [[]]
    filterOxygen [n]
      = [n]
    filterOxygen nums
      = map (digit : ) $ filterOxygen newNums
      where
        n = length nums
        oneCount =  sum $ map head nums
        digit = if oneCount * 2 >= n then 1 else 0
        newNums = map tail $ filter (\num -> head num == digit) nums

    filterCO2 :: [[Int]] -> [[Int]]
    filterCO2 ([] : _)
      = [[]]
    filterCO2 [n]
      = [n]
    filterCO2 nums
      = map (digit : ) $ filterCO2 newNums
      where
        n = length nums
        zeroCount = n - sum (map head nums)
        digit = if zeroCount * 2 <= n then 0 else 1
        newNums = map tail $ filter (\num -> head num == digit) nums

main :: IO ()
main
  = do
      nums <- getInput "input.txt"
      print $ partTwo nums