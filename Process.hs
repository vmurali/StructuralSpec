module Process(processFile) where

import Text.ParserCombinators.Parsec.Prim

import Lexer
import Preprocess
import ParseFile
import Options
import DataTypes
import PrintFile
import System.Directory
import System.IO
import System.Exit
import Data.List
import Data.Maybe

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

processFile options seenInterfacesIO file = do
  seenInterfaces <- seenInterfacesIO
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
          let imports = [x | Import x <- elements, isNothing (find (\(file, ifc) -> x == file) seenInterfaces)]
          let interfaces = [x | x@(Interface {}) <- elements]
          newInterfaces <- foldl (processFile options) (return $ (file, interfaces):seenInterfaces) imports
          let fullInterfacesList = (file, interfaces):newInterfaces
          writeFile (outPath ++ ".bsv") $ printFile elements fullInterfacesList
          return fullInterfacesList
  where
    outPath = optOutDir options ++ "/" ++ file
