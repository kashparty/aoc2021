getInput :: String -> IO [Int]
getInput fileName
  = do
      contents <- readFile fileName
      return $ read $ "[" ++ contents ++ "]"

prefixSum :: [Int] -> Int -> [Int]
prefixSum [] total
  = []
prefixSum (x : xs) total
  = newTotal : prefixSum xs newTotal
  where
    newTotal = total + x

partOne :: [Int] -> Int 
partOne nums
  = minimum $ zipWith (+) leftCosts rightCosts
  where
    minNums = minimum nums
    maxNums = maximum nums
    counts = [length $ filter (== i) nums | i <- [minNums..maxNums]]
    leftCosts = 0 : prefixSum (prefixSum counts 0) 0
    rightCosts = tail $ reverse $ prefixSum (prefixSum (reverse counts) 0) 0

partTwo :: [Int] -> Int 
partTwo nums
  = minimum $ zipWith (+) leftCosts rightCosts
  where
    minNums = minimum nums
    maxNums = maximum nums
    counts = [length $ filter (== i) nums | i <- [minNums..maxNums]]
    leftCosts = 0 : prefixSum (prefixSum (prefixSum counts 0) 0) 0
    rightCosts = 
      tail $ reverse $ prefixSum (prefixSum (prefixSum (reverse counts) 0) 0) 0

main :: IO ()
main
  = do
      nums <- getInput "input.txt"
      print $ partOne nums
      print $ partTwo nums