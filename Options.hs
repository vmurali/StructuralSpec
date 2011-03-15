module Options(Options(..), parserOpts) where

import System.Console.GetOpt
import Text.Regex
import System
import System.IO
import System.Directory

data Options = Options
  { optIncludes :: [String]
  , optOutDir   :: String
  , optElastic  :: Bool
  , optFile     :: String
  }

defaultOptions = Options
  { optIncludes = ["."]
  , optOutDir = "."
  , optElastic = False
  , optFile = ""
  }

splitColon str = splitRegex (mkRegex ":") str

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['i'] ["include"]
      (ReqArg (\inc opts -> return opts {optIncludes = splitColon inc}) "")
      "Include Paths"
  , Option ['o'] ["outdir"]
      (ReqArg (\out opts -> createDirectoryIfMissing True out >> return opts{optOutDir = out}) "")
      "Output directory"
  , Option ['e'] ["elastic"]
      (NoArg (\opts -> return opts{optElastic = True}))
      "Make elastic wrappers"
  , Option ['h'] ["help"]
      (NoArg (\_ -> do{prg <- getProgName; hPutStrLn stderr (usageInfo prg options); exitWith ExitSuccess}))
      "Show help"
  ]

parserOpts args =
  foldl (>>=) (return defaultOptions{optFile = head fileList}) optionList
  where
    (optionList, fileList, err) = getOpt RequireOrder options args
