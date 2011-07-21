module Process(process) where

import Text.ParserCombinators.Parsec
import System.Directory
import System.IO
import System.Exit
import Data.List
import Data.Maybe
import Control.Monad

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

process options seenPortsAliasesIO file = do
  seenPortsAliases <- seenPortsAliasesIO
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
          let imports = [x | Include x <- elements, isNothing (find (\(file, _) -> x == file) seenPortsAliases)]
          let ports = [x | x@(Port {}) <- elements]
          newPortsAliases <- foldl (process options) (return $ (file, ports):seenPortsAliases) imports
          let fullPortsAliasesList = (file, ports):newPortsAliases
          let bsvName = optOutDir options ++ "/" ++ file ++ ".bsv"
          let writeBsv = writeFile bsvName $ printFile fullPortsAliasesList elements
          bsvExists <- doesFileExist bsvName
          if bsvExists
            then do
              specModTime <- getModificationTime name
              bsvModTime <- getModificationTime bsvName
              when (specModTime >= bsvModTime || optForce options) writeBsv
            else writeBsv
          return fullPortsAliasesList
