module Ls where

import Lib
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Lens.Micro.Platform
import Data.List (sort, sortBy, intercalate)
import Data.Char (toLower)


type FilterFunc = (String -> Bool)
type SortFunc   = (String -> String -> Ordering)
type ViewFunc   = ([String] -> [String])

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

ls :: Args -> Command
ls []              = ls' [] defaultFilter defaultSort defaultView
ls (('-':str):[])  = ls' str defaultFilter defaultSort defaultView
ls ("--help":_)    = \inp -> (output .~ ["ls: show content of directory"]) <$> retNewStat inp
ls ("-h":rest)     = ls ("--help":rest)
ls ("--version":_) = \inp -> (output .~ ["pwd 1.0 \nWritten by Tselnv"])   <$> retNewStat inp
ls ("-v":rest)     = ls ("--version":rest)


ls' :: String -> FilterFunc ->  SortFunc -> ViewFunc -> Command
ls' [] ff sf vf          = \inp -> do
                           outResult <- lsCommon (fst inp) ff sf vf
                           (output .~ outResult) <$> retNewStat inp
ls' ('l':rest)  ff sf _  = ls' rest ff sf (\files -> files) -- yep, view func is id
ls' ('a':rest)  _  sf vf = ls' rest (\_ -> True) sf vf
ls' ('r':rest)  ff _  vf = ls' rest ff (\w1 w2 -> (map toLower w2) `compare` (map toLower w1)) vf


defaultFilter :: FilterFunc
defaultFilter = (\f -> head f /= '.')

defaultSort :: SortFunc
defaultSort = (\w1 w2 -> (map toLower w1) `compare` (map toLower w2))

defaultView :: ViewFunc
defaultView = (\fs -> [intercalate " " fs])



lsCommon :: FilePath -> FilterFunc -> SortFunc -> ViewFunc -> IO ([String])
lsCommon dir filterF sortF viewF =
   getDirectoryContents dir >>= \fs ->
     let fs1 = filter filterF fs
         fs2 = sortBy sortF fs1
         fs3 = viewF fs2
     in return fs3
                   
-- & action .~ (putStrLn $ stat ^. currentDir)
