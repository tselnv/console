module Tar (tar) where

import Common
import System.IO
import Lens.Micro.Platform
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, doesDirectoryExist, createDirectoryIfMissing)
import Data.Int (Int64)
import Control.Monad (when)
import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as Bs

type Name = String
type ArchiveName = FilePath
type Content = B.ByteString

data FSElemHeader = DirHeader Name | FileHeader Name Int64 deriving (Eq, Ord, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

tar :: Args -> Command
--tar []              = ls' [] [] defaultFilter defaultSort defaultView
tar ("--help":_)    = \inp -> (output .~ ["tar: archive directory to a file"]) <$> retNewStat inp
tar ("-h":rest)     = tar ("--help":rest)
tar ("--version":_) = \inp -> (output .~ ["tar 1.0 \nWritten by Tselnv"])   <$> retNewStat inp
tar ("-v":rest)     = tar ("--version":rest)
tar args            = tar' args



unarchiver :: FilePath -> FilePath -> IO ()
unarchiver archFile topDir = do
  archH <- openFile archFile ReadMode
  unarchLoop archH topDir
  hClose archH


unarchLoop :: Handle -> FilePath -> IO ()
unarchLoop archH topDir = do
  eof <- hIsEOF archH
  if eof
    then return ()
    else do
        str <- B.fromStrict <$> (Bs.hGetLine archH)
        let elem = readElemFromBS str
        case elem of
          Nothing -> putStrLn $ "Corrupted archive file " ++ BC.unpack str
          (Just (DirHeader name)) -> do
                createDirectoryIfMissing True $ topDir </> name
                unarchLoop archH topDir
          (Just (FileHeader name contentLen)) -> do
                content <- B.hGet archH (fromIntegral contentLen)
                B.writeFile (topDir </> name) content
                Bs.hGet archH 1 -- skip '\n'
                unarchLoop archH topDir
        return ()



readElemFromBS :: B.ByteString -> Maybe FSElemHeader
readElemFromBS bstr = maximum [readElemDir bstr, readElemFile bstr]


readElemFile :: B.ByteString -> Maybe FSElemHeader
readElemFile bstr = parseFileName bstr    >>= \(name,bstr') ->
                    parseContentLen bstr' >>= \clen         ->
                    return (FileHeader name clen)
                                     

parseFileName :: B.ByteString -> Maybe (Name, B.ByteString)
parseFileName bstr
  | B.length bstr < 6 = Nothing -- length "File 0"
  | B.take 5 bstr == (BC.pack "File ") = do
      let bslist = B.splitWith (==32) bstr -- 32 is space char ' '
      (dh:num:_) <- Just bslist
      let headerLen = B.length dh + 1 + B.length num + 1 -- +2 spaces
      nameLen <- (readMaybe (BC.unpack $ num) :: Maybe Int64)
      True <- Just (B.length bstr >= (headerLen + nameLen + 1)) -- if False then Nothing
      let name = BC.unpack $
                 B.take nameLen $
                 B.drop headerLen bstr
      let bstrLeft = B.drop (headerLen + nameLen + 1) bstr
      return (name, bstrLeft)
  | otherwise = Nothing


parseContentLen :: B.ByteString -> Maybe Int64
parseContentLen bstr = readMaybe $ BC.unpack bstr
  

readElemDir :: B.ByteString -> Maybe FSElemHeader
readElemDir bstr
  | B.length bstr < 5 = Nothing -- length "Dir 0"
  | bstr == BC.pack "Dir 0" = Just (DirHeader "")
  | B.take 4 bstr == (BC.pack "Dir ") = do
      let bslist = B.splitWith (==32) bstr -- 32 is space char ' '
      (dh:num:_) <- Just bslist
      let headerLen = B.length dh + 1 + B.length num + 1 -- +2 spaces
      nameLen <- (readMaybe $ BC.unpack $ num :: Maybe Int64)
      True <- Just (B.length bstr == (headerLen + nameLen)) -- if False then Nothing
      let dirName = BC.unpack $
                      B.take nameLen $
                      B.drop headerLen bstr
      return (DirHeader dirName)
  | otherwise = Nothing


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


-- format :: Dir length_of_relative_dir_name relative_dir_name\n
-- format :: File length_of_relative_file_name relative_file_name end_of_handle_position_of_content\n
archiver :: ArchiveName -> FilePath -> IO ()
archiver archiveFileName dir = do
  archH <- openFile archiveFileName WriteMode
  archDir archH dir ""
  hClose archH

  
archDir :: Handle -> FilePath -> FilePath -> IO ()
archDir archH topDir relativeDir = do
  if null relativeDir
    then B.hPut archH $ BC.pack "Dir 0\n"
    else B.hPut archH $ BC.pack $ concat [ "Dir "
                                         , show $ B.length $ BC.pack relativeDir
                                         , " "
                                         , relativeDir
                                         , "\n"]
  rawFiles <- getDirectoryContents (topDir </> relativeDir)
  let files = filter (`notElem` [".",".."]) rawFiles
  mapM filesProcessor (map (relativeDir </> )  files)
  return ()
  where filesProcessor :: Name -> IO ()
        filesProcessor relFileName = do
          exist <- doesDirectoryExist ( topDir </> relFileName)
          if exist
            then archDir archH topDir relFileName
            else archFile archH topDir relFileName


archFile :: Handle -> FilePath -> Name -> IO ()
archFile archH topDir relativeFileName = do
  content <- B.readFile $ topDir </> relativeFileName
  let contentLength = B.length content
  hPosition <- hTell archH
  let headerByteStr = BC.pack $ concat [ "File "
                                       , show $ B.length $ BC.pack relativeFileName
                                       , " "
                                       , relativeFileName
                                       , " "]
  let endContentPosition = hPosition +
                           (fromIntegral $ B.length headerByteStr) +
                           (fromIntegral $ B.length content) + 1
  B.hPut archH  $ B.concat [ headerByteStr
                           , BC.pack $ show contentLength
                           , BC.pack "\n"
                           , content
                           , BC.pack "\n"]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

parseArgs :: [String] -> IO (IO (), [String])
parseArgs ("-a":archFile:dir:[]) = do
                                   exist <- doesDirectoryExist dir
                                   if exist
                                     then return (archiver archFile dir, [])
                                     else return (return (), ["directory " ++ dir ++ " doesn't exist"])
parseArgs ("-e":archFile:dir:[]) = do
                                   exist <- doesFileExist archFile
                                   if exist
                                     then return (unarchiver archFile dir, [])
                                     else return (return (), ["file " ++ archFile ++ " doesn't exist"])
parseArgs _ = return (return (), ["usage to make archive    : tar -a archive_file_to_create directory_to_archive"
                                 ,"      to exract directory: tar -e archive_file_to_extract directory_to_create"])


tar' :: [String] -> Command
tar' args = \inp -> do
  (act, outResult) <- parseArgs args
  ((output .~ outResult) . (action .~ act)) <$> retNewStat inp


  -- args <- getArgs
  -- parseArgs args


-- ls' [] [] ff sf vf               = \inp -> do
--                                            outResult <- lsCommon (fst inp) ff sf vf
--                                            (output .~ outResult) <$> retNewStat inp

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
