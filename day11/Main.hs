import Debug.Trace
import Data.List
getInput :: String -> IO [[Int]]
getInput fileName
  = do
      contents <- readFile fileName
      let ls = lines contents
      return $ map (map (\c -> read [c])) ls

toString :: [[(Int, Bool)]] -> String
toString grid
  = intercalate "\n" (map (concatMap (\x -> show (fst x) ++ " ")) grid) ++ "\n"

newValue :: [[(Int, Bool)]] -> Int -> Int -> (Int, Bool)
newValue before thisR thisC
  | f || v == 0         = (0, False)
  | v + sumFlashing > 9 = (0, True)
  | otherwise           = (v + sumFlashing, False)
  where
    (v, f) = before !! thisR !! thisC
    sumFlashing =
      sum $ concat [
        [
          if snd (before !! r !! c) then 1 else 0
          | c <- [max 0 (thisC - 1)..min 9 (thisC + 1)],
          not (r == thisR && c == thisC)
        ]
        | r <- [max 0 (thisR - 1)..min 9 (thisR + 1)]
      ]

runFlashes :: [[(Int, Bool)]] -> [[(Int, Bool)]]
runFlashes before
  | before == after = after
  | otherwise       = runFlashes after
  where
    after = [[newValue before r c | c <- [0..9]] | r <- [0..9]]

partOne :: [[Int]] -> Int -> Int
partOne _ 0
  = 0
partOne grid steps
  = flashes + partOne newGrid (steps - 1)
  where
    increment = map (map (1 + )) grid
    justFlashed = [[False | _ <- [1..10]] | _ <- [1..10]]

    beforeData = zipWith zip increment justFlashed
    afterData = runFlashes beforeData
    
    newGrid = [[if x > 9 then 0 else x | (x, _) <- row] | row <- afterData]
    flashes =
      sum $ concat [[if x == 0 then 1 else 0 | x <- row] | row <- newGrid]

partTwo :: [[Int]] -> Int -> Int
partTwo grid steps
  | flashes == 100 = steps + 1
  | otherwise      = partTwo newGrid (steps + 1)
  where
    increment = map (map (1 + )) grid
    justFlashed = [[False | _ <- [1..10]] | _ <- [1..10]]

    beforeData = zipWith zip increment justFlashed
    afterData = runFlashes beforeData
    
    newGrid = [[if x > 9 then 0 else x | (x, _) <- row] | row <- afterData]
    flashes =
      sum $ concat [[if x == 0 then 1 else 0 | x <- row] | row <- newGrid]

main :: IO ()
main
  = do
      grid <- getInput "input.txt"
      print $ partOne grid 100 
      print $ partTwo grid 0