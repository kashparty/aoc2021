data Command = Forward Int | Down Int | Up Int

getInput :: String -> IO [Command]
getInput fileName
  = do
      contents <- readFile fileName
      return $ map parseCommand $ lines contents
    where
      parseCommand :: String -> Command
      parseCommand command
        | commandType == "forward" = Forward sizeInt
        | commandType == "down"    = Down sizeInt
        | commandType == "up"      = Up sizeInt
        | otherwise                = error commandType
        where
          (commandType : size : _) = words command
          sizeInt = read size :: Int

partOne :: [Command] -> (Int, Int)
partOne []
  = (0, 0)
partOne (Forward size : cmds)
  = (x + size, y)
  where
    (x, y) = partOne cmds
partOne (Down size : cmds)
  = (x, y + size)
  where
    (x, y) = partOne cmds
partOne (Up size : cmds)
  = (x, y - size)
  where
    (x, y) = partOne cmds

partTwo :: [Command] -> (Int, Int, Int)
partTwo []
  = (0, 0, 0)
partTwo (Forward size : cmds)
   = (x + size, y + (a * size), a)
   where
     (x, y, a) = partTwo cmds
partTwo (Up size : cmds)
  = (x, y, a - size)
  where
    (x, y, a) = partTwo cmds
partTwo (Down size : cmds)
  = (x, y, a + size)
  where
    (x, y, a) = partTwo cmds

main :: IO ()
main
  = do
      cmds <- getInput "input.txt"
      let (x, y) = partOne cmds
      print (x * y)
      let (x, y, _) = partTwo $ reverse cmds
      print (x * y)