import Text.Regex

import Options
import Process
import System.Environment

removeSpec str = subRegex (mkRegex "\\.spec$") str ""
removePrefix str = subRegex (mkRegex "^.*\\/") str ""

main = do
  args <- getArgs
  opts <- parserOpts args
  process opts (return []) $ (removeSpec . removePrefix) (optFile opts)
