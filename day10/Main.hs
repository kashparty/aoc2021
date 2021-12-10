import Data.List
import Data.Maybe

getInput :: String -> IO [String]
getInput fileName
  = do
      contents <- readFile fileName
      return $ lines contents

closer :: Char -> Char
closer '('
  = ')'
closer '['
  = ']'
closer '{'
  = '}'
closer '<'
  = '>'

getCorruption :: String -> String -> (String, Bool)
getCorruption [] stack
  = (stack, False)
getCorruption (c : cs) stack
  | c `elem` ['(', '[', '{', '<'] = getCorruption cs (closer c : stack)
  | c == head stack               = getCorruption cs (tail stack)
  | otherwise                     = ([c], True)

partOne :: [String] -> Int
partOne ls
  = sum $ map lineToScore ls
  where
    lineToScore :: String -> Int
    lineToScore l
      | corrupted = charToScore $ head result
      | otherwise = 0
      where
        (result, corrupted) = getCorruption l []

    charToScore :: Char -> Int
    charToScore ')'
      = 3
    charToScore ']'
      = 57
    charToScore '}'
      = 1197
    charToScore '>'
      = 25137
    charToScore _
      = 0

partTwo :: [String] -> Int
partTwo ls
  = sort scores !! (length scores `div` 2) 
  where
    ends = map fst $ filter (\(c, b) -> not b) $ map (`getCorruption` []) ls
    scores = map (`completionScore` 0) ends

    completionScore :: String -> Int -> Int
    completionScore [] score
      = score
    completionScore (c : cs) score
      = completionScore cs newScore
      where
        newScore = 5 * score + fromJust (c `elemIndex` [')', ']', '}', '>']) + 1

main :: IO ()
main
  = do
      ls <- getInput "input.txt"
      print $ partOne ls
      print $ partTwo ls