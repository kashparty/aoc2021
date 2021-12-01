getInput :: String -> IO [Int]
getInput filename
  = do
      content <- readFile filename
      return ((map read $ lines content) :: [Int])

partOne :: [Int] -> Int 
partOne nums
  = sum $ zipWith (\a b -> if a < b then 1 else 0) nums nexts
  where
    nexts = tail nums

partTwo :: [Int] -> Int 
partTwo firsts
  = partOne sums
  where
    seconds = tail firsts
    thirds = tail seconds
    sums = zipWith3 (\a b c -> a + b + c) firsts seconds thirds

main :: IO()
main
  = do
      nums <- getInput "input.txt"
      let ans = partTwo nums
      print ans