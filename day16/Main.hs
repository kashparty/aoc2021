import Data.Char

data Packet = Literal Int Int | Operator Int Int [Packet]
            deriving (Show, Eq)

getInput :: String -> IO [Int]
getInput fileName
  = do
      contents <- readFile fileName
      return $ concatMap hexToBin contents

intToBin :: Int -> [Int]
intToBin n
  = reverse $ intToBin' n
  where
    intToBin' :: Int -> [Int]
    intToBin' 0
      = []
    intToBin' n
      = (n `mod` 2) : intToBin' (n `div` 2)

binToInt :: [Int] -> Int
binToInt ns
  = binToInt' (dropWhile (== 0) ns) 0
  where
    binToInt' [] curr
      = curr
    binToInt' (n : ns) curr
      = binToInt' ns (2 * curr + n)

hexToBin :: Char -> [Int]
hexToBin c
  = replicate (4 - length bin) 0 ++ bin
  where
    int = if isDigit c then ord c - ord '0' else 10 + ord c - ord 'A'
    bin = intToBin int

parsePacket :: [Int] -> (Packet, [Int])
parsePacket (v1 : v2 : v3 : 1 : 0 : 0 : rest)
  = (Literal version (binToInt value), restAfterLiteral)
  where
    version = binToInt [v1, v2, v3]
    (value, restAfterLiteral) = parseLiteral rest

    parseLiteral :: [Int] -> ([Int], [Int])
    parseLiteral (1 : b1 : b2 : b3 : b4 : rest)
      = ([b1, b2, b3, b4] ++ restDigits, restAfterLiteral)
      where
        (restDigits, restAfterLiteral) = parseLiteral rest
    parseLiteral (0 : b1 : b2 : b3 : b4 : rest)
      = ([b1, b2, b3, b4], rest)
parsePacket (v1 : v2 : v3 : t1 : t2 : t3 : 0 : rest)
  = (Operator version typeId subPackets, afterSubPackets)
  where
    version = binToInt [v1, v2, v3]
    typeId = binToInt [t1, t2, t3]
    (toParseBits, afterLength) = splitAt 15 rest
    toParse = binToInt toParseBits
    (subPackets, afterSubPackets) = collectSubPackets afterLength toParse

    collectSubPackets :: [Int] -> Int -> ([Packet], [Int])
    collectSubPackets bits 0
      = ([], bits)
    collectSubPackets bits toParse
      = (nextPacket : subPackets, rest) 
      where
        (nextPacket, nextBits) = parsePacket bits
        parsed = length bits - length nextBits
        (subPackets, rest) = collectSubPackets nextBits (toParse - parsed)
parsePacket (v1 : v2 : v3 : t1 : t2 : t3 : 1 : rest)
  = (Operator version typeId subPackets, afterSubPackets)
  where
    version = binToInt [v1, v2, v3]
    typeId = binToInt [t1, t2, t3]
    (toParseBits, afterLength) = splitAt 11 rest
    toParse = binToInt toParseBits
    (subPackets, afterSubPackets) = collectSubPackets afterLength toParse

    collectSubPackets :: [Int] -> Int -> ([Packet], [Int])
    collectSubPackets bits 0
      = ([], bits)
    collectSubPackets bits toParse
      = (nextPacket : subPackets, rest)
      where
        (nextPacket, nextBits) = parsePacket bits
        (subPackets, rest) = collectSubPackets nextBits (toParse - 1)

partOne :: [Int] -> Int
partOne bits
  = sumVersions mainPacket 
  where 
    (mainPacket, _) = parsePacket bits

    sumVersions :: Packet -> Int
    sumVersions (Literal v _)
      = v
    sumVersions (Operator v _ ps)
      = v + sum (map sumVersions ps)

partTwo :: [Int] -> Int
partTwo bits
  = evaluate mainPacket
  where
    (mainPacket, _) = parsePacket bits

    evaluate :: Packet -> Int
    evaluate (Literal _ v)
      = v
    evaluate (Operator _ 0 ps)
      = sum (map evaluate ps)
    evaluate (Operator _ 1 ps)
      = product (map evaluate ps)
    evaluate (Operator _ 2 ps)
      = minimum (map evaluate ps)
    evaluate (Operator _ 3 ps)
      = maximum (map evaluate ps)
    evaluate (Operator _ 5 [p1, p2])
      | evaluate p1 > evaluate p2 = 1
      | otherwise                 = 0
    evaluate (Operator _ 6 [p1, p2])
      | evaluate p1 < evaluate p2 = 1
      | otherwise                 = 0
    evaluate (Operator _ 7 [p1, p2])
      | evaluate p1 == evaluate p2 = 1
      | otherwise                  = 0

main :: IO ()
main
  = do
      bits <- getInput "input.txt"
      print $ partOne bits
      print $ partTwo bits