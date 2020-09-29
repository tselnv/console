module Common where


import System.Directory (doesDirectoryExist, getDirectoryContents)
import Lens.Micro.Platform
import Data.List (sort, sortBy, intercalate)
import Data.Char (toLower)


type Args = [String]
type Command = InputStat -> IO (Status)

type Output = [String]

type InputStat = (FilePath, Output)

data Status = Status FilePath Output (IO()) Bool

output :: Lens' Status Output
output = lens (\(Status _ out _ _)             -> out)
              (\(Status dir _ act flag) newOut -> Status dir newOut act flag)

currentDir :: Lens' Status FilePath
currentDir = lens (\(Status dir _ _ _)             -> dir)
                  (\(Status _ out act flag) newDir -> Status newDir out act flag)

continueFlag :: Lens' Status Bool
continueFlag = lens (\(Status _ _ _ flag)             -> flag)
                    (\(Status dir out act _) newFlag  -> Status dir out act newFlag)

action :: Lens' Status (IO())
action = lens (\(Status _ _ act _)              -> act)
              (\(Status dir out _ flag) newAct  -> Status dir out newAct flag)

inputStat :: Status -> InputStat
inputStat st = (st^.currentDir, st^.output)
                                 
newStat :: InputStat -> Status
newStat (dir, _) = Status dir [""] (return()) True

retNewStat :: InputStat -> IO (Status)
retNewStat inp = return $ newStat inp

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

unknownCommand :: String -> Command
unknownCommand s = \inp -> (output .~ ["unknown command: " ++ s]) <$> retNewStat inp


pass :: Command
pass = \inp -> retNewStat inp


exit :: Args -> Command
exit _ = \inp -> ((continueFlag .~ False) . (output .~ ["Exit"])) <$> retNewStat inp


pwd :: Args -> Command
pwd args = case args of
  "-h":_             -> help
  "--help":_         -> help
  "--version":_      -> version
  _                  -> \inp@(dir,_) -> (output .~ [dir]) <$> retNewStat inp
  where help    = \inp -> (output .~ ["pwd: print current directory"]) <$> retNewStat inp
        version = \inp -> (output .~ ["pwd 1.0 \nWritten by Tselnv"])  <$> retNewStat inp
               


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


simplifyArgs :: String -> [String]
simplifyArgs = words .  unwords . map checkMinuses . words
  where checkMinuses all@('-':'-':rest) = all
        checkMinuses ('-':rest)         = splitMinus rest
        checkMinuses str                = str

-- double unwords . words . unwords to remove extra spaces

splitMinus :: String -> String
splitMinus (ch:rest) = '-':ch:' ':(splitMinus rest)
splitMinus [] = []
