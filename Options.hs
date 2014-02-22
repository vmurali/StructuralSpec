module Options(Options(..), parserOpts) where

import System.Environment
import System.Exit
import System.Console.GetOpt
import Text.Regex
import System.IO
import System.Directory

data Options = Options
  { optIncludes :: [String]
  , optOutDir   :: String
  , optFile     :: String
  , optForce    :: Bool
  }

defaultOptions = Options
  { optIncludes = ["."]
  , optOutDir = "."
  , optFile = ""
  , optForce = False
  }

splitColon str = splitRegex (mkRegex ":") str

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['i'] ["include"]
      (ReqArg (\inc opts -> return opts {optIncludes = optIncludes opts ++ splitColon inc}) "")
      "Include Paths"
  , Option ['o'] ["outdir"]
      (ReqArg (\out opts -> createDirectoryIfMissing True out >> return opts{optOutDir = out}) "")
      "Output directory"
  , Option ['f'] ["force"]
      (NoArg (\opts -> return opts {optForce = True}))
      "Force recompile"
  , Option ['h'] ["help"]
      (NoArg (\_ -> do{prg <- getProgName; hPutStrLn stderr (usageInfo prg options); exitWith ExitSuccess}))
      "Show help"
  ]

parserOpts args =
  foldl (>>=) (return defaultOptions{optFile = head fileList}) optionList
  where
    (optionList, fileList, err) = getOpt RequireOrder options args
