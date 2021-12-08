import Data.Text (splitOn, pack, unpack)
import Data.Maybe
import Data.List

type Line = ([String], [String])

parseLine :: String -> Line
parseLine line
  = (words . unpack $ left, words . unpack $ right)
  where
    (left : right : _) = splitOn (pack " | ") (pack line)

getInput :: String -> IO [Line]
getInput fileName
  = do
      contents <- readFile fileName
      return $ map parseLine $ lines contents

partOne :: [Line] -> Int
partOne []
  = 0
partOne ((_, digits) : rest)
  = count + partOne rest
  where
    count = length $ filter numSegsUnique digits

    numSegsUnique :: String -> Bool
    numSegsUnique s
      = length s `elem` [2, 4, 3, 7]

partTwo :: [Line] -> Int
partTwo ls
  = sum $ map lineToOutput ls 
  where
    realCharOccs = 
      [('a', 8), ('b', 6), ('c', 8), ('d', 7), ('e', 4), ('f', 9), ('g', 7)]
    digitChars = 
      ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
      "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

    toOccs charOccs = map (sort . map (fromJust . (`lookup` charOccs)))
    digitMap = zip (toOccs realCharOccs digitChars) [0..9]

    lineToOutput :: Line -> Int
    lineToOutput (left, right)
      = digitsToInt $ map (fromJust . (`lookup` mapping) . sort) right
      where
        charOccs' = [(c, length $ filter (c `elem`) left) | c <- ['a'..'g']]
        digitOccs' = toOccs charOccs' left
        mapping = 
          zip (map sort left) $ map (fromJust . (`lookup` digitMap)) digitOccs' 

        digitsToInt :: [Int] -> Int
        digitsToInt []
          = 0
        digitsToInt (x : xs)
          = x * 10 ^ length xs + digitsToInt xs
        
main :: IO ()
main
  = do
      ls <- getInput "input.txt"
      print $ partOne ls
      print $ partTwo ls