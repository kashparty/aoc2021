import Data.List
import Data.Maybe
getInput :: String -> IO ((Int, Int), (Int, Int))
getInput fileName
  = do
      contents <- readFile fileName
      let 
        coordinates 
          = tail $ dropWhile (/= ':') contents
        (_ : _ : _ : xCoordinates, _ : _ : _ : _ : yCoordinates) 
          = splitAt (fromJust $ ',' `elemIndex` coordinates) coordinates
        (x1, _ : _ : x2) 
          = splitAt (fromJust $ '.' `elemIndex` xCoordinates) xCoordinates
        (y1, _ : _ : y2) 
          = splitAt (fromJust $ '.' `elemIndex` yCoordinates) yCoordinates
      return ((read x1, read x2), (read y1, read y2))

partOne :: ((Int, Int), (Int, Int)) -> Int
partOne ((_, _), (y1, _))
  = y1' * (y1' + 1) `div` 2 
  where
    y1' = -y1 - 1

simX :: (Int, Int) -> Int -> Int -> Int -> Int -> [Int]
simX (x1, x2) x vx t maxYT
  | x >= x1 && x <= x2 && vx /= 0 = t : nextSim
  | x >= x1 && x <= x2            = [t..maxYT]
  | vx == 0                       = [] 
  | x > x2                        = []
  | otherwise                     = nextSim
  where
    nextSim = simX (x1, x2) (x + vx) (max 0 (vx - 1)) (t + 1) maxYT

simY :: (Int, Int) -> Int -> Int -> Int -> [Int] 
simY (y1, y2) y vy t
  | y >= y1 && y <= y2 = t : simY (y1, y2) (y + vy) (vy - 1) (t + 1)
  | y < y1 && vy <= 0  = [] 
  | otherwise          = simY (y1, y2) (y + vy) (vy - 1) (t + 1)

partTwo :: ((Int, Int), (Int, Int)) -> Int
partTwo coords@((x1, x2), (y1, y2))
  = length uniquePoss
  where
    vx1 = 0
    vx2 = x2
    vy1 = y1
    vy2 = -y1 - 1 
    vys = concat [[(vy, ty) | ty <- simY (y1, y2) 0 vy 0] | vy <- [vy1..vy2]]
    maxYT = maximum $ map snd vys
    vxs = concat [[(vx, tx) 
                  | tx <- simX (x1, x2) 0 vx 0 maxYT] 
                  | vx <- [vx1..vx2]]
    poss = concat [[(vx, vy) | (vy, ty) <- vys, ty == tx] | (vx, tx) <- vxs]
    uniquePoss = map head . group $ sort poss

main :: IO ()
main
  = do
      bounds <- getInput "input.txt"
      print $ partOne bounds 
      print $ partTwo bounds