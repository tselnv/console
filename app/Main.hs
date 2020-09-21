-- {-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import Ls
import Control.Monad (when)
import System.Directory (getCurrentDirectory)
import System.IO (hFlush, stdout)
import Lens.Micro.Platform


parseCommand :: [String] -> Command
parseCommand ("exit":rest) = exit rest
parseCommand ("e":rest) = exit rest
parseCommand ("pwd":rest)  = pwd rest
parseCommand ("ls":rest)  = ls rest
parseCommand (x:rest) = unknownCommand x
parseCommand [] = pass


commandLoop :: Status -> IO ()
commandLoop status = do
  putStr ">> "
  hFlush stdout -- this command needs because of ghci disables buffering
  input <- getLine
  let command = parseCommand $ words input
  newStatus <- command $ inputStat status
  newStatus^.action
  let out = newStatus^.output in
    when (not $ null out)  (mapM_ putStrLn out)
  if newStatus^.continueFlag
    then commandLoop newStatus
    else return ()
    

main :: IO ()
main = getCurrentDirectory >>= \dir -> commandLoop $ newStat (dir, [""])
