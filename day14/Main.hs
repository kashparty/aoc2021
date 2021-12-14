import Data.Text (pack, unpack, splitOn)
import Data.List

parseRule :: String -> (String, Char)
parseRule rule
  = (unpack pattern, insertChar)
  where
    (pattern : insert : _) = splitOn (pack " -> ") (pack rule)
    [insertChar] = unpack insert

getInput :: String -> IO (String, [(String, Char)])
getInput fileName
  = do
      contents <- readFile fileName
      let (template : rules : _) = splitOn (pack "\n\n") (pack contents)
      return (unpack template, map parseRule $ lines (unpack rules))

partOne :: String -> [(String, Char)] -> Int
partOne template rules
  = maxOcc - minOcc
  where
    final = step template rules 10
    occurrences = map length . group $ sort final
    maxOcc = maximum occurrences
    minOcc = minimum occurrences

    step :: String -> [(String, Char)] -> Int -> String
    step template _ 0
      = template
    step template rules steps
      = step result rules (steps - 1)
      where
        adjacents = zipWith (\a b -> [a, b]) template (tail template ++ " ")
        inserts = map (`lookup` rules) adjacents
        result = concat $ zipWith insertChar template inserts

        insertChar :: Char -> Maybe Char -> String
        insertChar t Nothing
          = [t]
        insertChar t (Just c)
          = [t, c]

merge :: Eq a => Ord a => [(a, Int)] -> [(a, Int)]
merge before
  = map (\ps@((a, _) : _) -> (a, sum $ map snd ps)) groups 
  where
    groups = groupBy (\(a, b) (c, d) -> a == c) $ sort before

partTwo :: String -> [(String, Char)] -> Int
partTwo template rules
  = maxOcc - minOcc 
  where
    adjacents = zipWith (\a b -> [a, b]) template (tail template ++ " ")
    pairs = zip adjacents (repeat 1)

    beforePairs = merge $ zip adjacents (repeat 1) 
    afterPairs = step beforePairs rules 40

    occurrences = map (\(a : _, b) -> (a, b)) afterPairs
    groupedOccurrences = map snd $ merge occurrences

    maxOcc = maximum groupedOccurrences
    minOcc = minimum groupedOccurrences

    step :: [(String, Int)] -> [(String, Char)] -> Int -> [(String, Int)]
    step pairs _ 0
      = pairs
    step pairs rules steps
      = step result rules (steps - 1)
      where
        inserts = map (\(p, _) -> p `lookup` rules) pairs
        newPairs = concat $ zipWith insertChar pairs inserts
        result = merge newPairs 

        insertChar :: (String, Int) -> Maybe Char -> [(String, Int)]
        insertChar p Nothing
          = [p]
        insertChar ([a, b], n) (Just c)
          = [([a, c], n), ([c, b], n)]

main :: IO ()
main
  = do
      (template, rules) <- getInput "input.txt"
      print $ partOne template rules
      print $ partTwo template rules