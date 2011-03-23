module Process(process) where

import Text.ParserCombinators.Parsec.Prim
import System.Directory
import System.IO
import System.Exit
import Data.List
import Data.Maybe

import Lexer
import Preprocess
import ParseFile
import Options
import DataTypes
import PrintFile

getFilePath file foundPathIO newPath = do
  foundPath <- foundPathIO
  case foundPath of
    Just path -> (return . Just) path
    Nothing -> do
      let newF = newPath ++ "/" ++ file ++ ".spec"
      fileExists <- doesFileExist newF
      if fileExists
        then (return . Just) newF
        else return Nothing

process options seenPortsIO file = do
  seenPorts <- seenPortsIO
  filePath <- foldl (getFilePath file) (return Nothing) $ optIncludes options
  case filePath of
    Nothing -> do
      hPutStrLn stderr $ "File " ++ file ++ " not found!"
      exitFailure
    Just name -> do
      input <- readFile name
      case (runParser (whiteSpace>>parseFile) () name $ preprocess input) of
        Left err -> do
          print err
          exitFailure
        Right elements -> do
          let imports = [x | Include x <- elements, isNothing (find (\(file, _) -> x == file) seenPorts)]
          let ports = [x | x@(Port {}) <- elements]
          newPorts <- foldl (process options) (return $ (file, ports):seenPorts) imports
          let fullPortsList = (file, ports):newPorts
          writeFile (outPath ++ ".bsv") $ printFile fullPortsList elements
          return fullPortsList
  where
    outPath = optOutDir options ++ "/" ++ file
