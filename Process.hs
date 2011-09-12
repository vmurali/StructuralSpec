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
          putStrLn $ "Error in file " ++ file
          print err
          exitFailure
        Right elements -> do
          let imports = [x | Include x <- elements, isNothing (find (\(file, _, _) -> x == file) seenPortsAliases)]
          let ports = [x | x@(Port {}) <- elements]
          let aliases = [x | x@(Alias {}) <- elements]
          fullPortsAliasesList <- foldl (process options) (return $ (file, ports, aliases):seenPortsAliases) imports
          let bsvName = optOutDir options ++ "/" ++ file ++ ".bsv"
          output <- printFile file fullPortsAliasesList elements
          let writeBsv = writeFile bsvName output
          bsvExists <- doesFileExist bsvName
          if bsvExists
            then do
              specModTime <- getModificationTime name
              bsvModTime <- getModificationTime bsvName
              when (specModTime >= bsvModTime || optForce options) writeBsv
            else writeBsv
          return fullPortsAliasesList
