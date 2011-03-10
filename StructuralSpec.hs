import System
import Text.Regex
import Options
import Process

removeSpec str = subRegex (mkRegex "\\.spec$") str ""
removePrefix str = subRegex (mkRegex "^[^A-Z]*") str ""

main = do
  args <- getArgs
  opts <- parserOpts args
  processFile opts (return []) $ (removeSpec . removePrefix) (optFile opts)
