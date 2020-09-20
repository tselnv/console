-- {-# LANGUAGE TemplateHaskell #-}

module Main where

-- import Lib
import System.Directory (getCurrentDirectory)
import Lens.Micro.Platform
import System.IO (hFlush, stdout)


type Args = [String]
type Command = Status -> Status


data Status = Status FilePath String Bool

output :: Lens' Status String
output = lens (\(Status _ out _)           -> out)
              (\(Status dir _ flag) newOut -> Status dir newOut flag)

currentDir :: Lens' Status FilePath
currentDir = lens (\(Status dir _ _)           -> dir)
                  (\(Status _ out flag) newDir -> Status newDir out flag)

exitFlag :: Lens' Status Bool
exitFlag = lens (\(Status _ _ flag)           -> flag)
                (\(Status dir out _) newFlag  -> Status dir out newFlag)

                                 

parseCommand :: [String] -> Command
parseCommand ("exit":rest) = exit rest
parseCommand ("pwd":rest)  = pwd rest


exit :: Args -> Command
exit arg = \stat -> stat & exitFlag .~ True & output .~ "Exit"

pwd :: Args -> Command
pwd args = case args of
  "-h":_             -> help
  "--help":_         -> help
  "--version":_      -> version
  _                  -> \stat -> stat & output .~ stat^.currentDir
  where help    = \stat -> stat & output .~ "pwd: print current directory"
        version = \stat -> stat & output .~ "pwd 1.0 \nWritten by Tselnv"
        

commandLoop :: Status -> IO ()
commandLoop status = do
  putStr ">> "
  hFlush stdout -- this command needs becouse of ghci disables buffering
  input <- getLine
  let command = parseCommand $ words input
  let newStatus = command status
  putStrLn $ newStatus^.output
  if not $ newStatus^.exitFlag
    then commandLoop newStatus
    else return ()
    
  

main :: IO ()
main = getCurrentDirectory >>= \dir -> commandLoop (Status dir "" False)
  
--
