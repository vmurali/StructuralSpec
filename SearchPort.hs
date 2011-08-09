module SearchPort(searchPort) where

import DataTypes
import Data.List

searchPort name filePortsAliases = foldl matchPort Nothing filePortsAliases
 where
  matchPort (Just found) _                      = Just found
  matchPort Nothing      (file, ports, aliases) = case (find (\x -> portName x == name) ports) of
                                                    Just found -> Just found
                                                    Nothing    -> searchAlias name filePortsAliases aliases

searchAlias name filePortsAliases aliases =
  case foundPort of
    Just found -> searchPort (aliasPort found) filePortsAliases
    Nothing    -> Nothing
 where
  foundPort = find (\x -> aliasName x == name) aliases
