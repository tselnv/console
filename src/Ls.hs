module Ls where

import Common
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Lens.Micro.Platform
import Data.List (sort, sortBy, intercalate)
import Data.Char (toLower)


type FilterFunc = (String -> Bool)
type SortFunc   = (String -> String -> Ordering)
type ViewFunc   = ([String] -> [String])

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


ls :: Args -> Command
ls []              = ls' [] [] defaultFilter defaultSort defaultView
ls ("--help":_)    = \inp -> (output .~ ["ls: show content of directory"]) <$> retNewStat inp
ls ("-h":rest)     = ls ("--help":rest)
ls ("--version":_) = \inp -> (output .~ ["pwd 1.0 \nWritten by Tselnv"])   <$> retNewStat inp
ls ("-v":rest)     = ls ("--version":rest)
ls args            = ls' args [] defaultFilter defaultSort defaultView



ls' :: [String] -> [FilePath] -> FilterFunc ->  SortFunc -> ViewFunc -> Command
ls' [] [] ff sf vf               = \inp -> do
                                           outResult <- lsCommon (fst inp) ff sf vf
                                           (output .~ outResult) <$> retNewStat inp
ls' [] dirs ff sf vf             = \inp -> do
                                      outResult <-lsSeveralDirs dirs ff sf vf
                                      (output .~ outResult) <$> retNewStat inp

ls' ("-l":rest) dirs ff sf _     = ls' rest dirs ff sf id -- view func is id, not defaultView
ls' ("-a":rest) dirs _  sf vf    = ls' rest dirs (\_ -> True) sf vf
ls' ("-r":rest) dirs ff _  vf    = ls' rest dirs ff (\w1 w2 -> (map toLower w2) `compare` (map toLower w1)) vf
ls' (('-':rest):_) _    _  _  _  = \inp -> (output .~ ["unknown argument -" ++ rest]) <$> retNewStat inp
ls' (d:rest)    dirs ff sf vf    = ls' rest (d:dirs) ff sf vf



defaultFilter :: FilterFunc
defaultFilter = (\f -> head f /= '.')

defaultSort :: SortFunc
defaultSort = (\w1 w2 -> (map toLower w1) `compare` (map toLower w2))

defaultView :: ViewFunc
defaultView = (\fs -> [intercalate " " fs])


lsSeveralDirs :: [FilePath] -> FilterFunc -> SortFunc -> ViewFunc -> IO ([String])
lsSeveralDirs (dir:[]) ff sf vf = lsCommon dir ff sf vf
lsSeveralDirs dirs     ff sf vf = concat <$> (sequence $ map applyLsCommon (reverse dirs))
    where applyLsCommon :: FilePath -> IO([String])
          applyLsCommon  = \d -> addNameOfDir d <$> (lsCommon d ff sf vf)
          addNameOfDir :: FilePath -> [String] -> [String]
          addNameOfDir d = \strs -> (d ++ ":"):strs ++ [""]

lsCommon :: FilePath -> FilterFunc -> SortFunc -> ViewFunc -> IO ([String])
lsCommon dir filterF sortF viewF =
   doesDirectoryExist dir       >>= \exst ->
   if exst
     then 
       getDirectoryContents dir >>= \fs   ->
         let fs1 = filter filterF fs
             fs2 = sortBy sortF fs1
             fs3 = viewF fs2
         in return fs3
     else
       return ["directory doesn`t exist"]
     

            
-- & action .~ (putStrLn $ stat ^. currentDir)
