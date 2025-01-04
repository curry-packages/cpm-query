------------------------------------------------------------------------------
--- Some operations to handle the `cpm-query` resource configuration file
--- that is stored in `$HOME/.cpmqueryrc`
---
--- @author  Michael Hanus
--- @version January 2025
------------------------------------------------------------------------------

module CPM.Query.RCFile
  ( readRC, rcValue )
 where

import Control.Monad     ( unless )
import Data.Char         ( toLower )
import Data.Either       ( rights )
import Data.List         ( intercalate, sort )
import System.IO         ( hPutStrLn, stderr )

import Data.PropertyFile ( readPropertyFile, updatePropertyFile )
import System.FilePath   ( FilePath, (</>), (<.>) )
import System.Directory  ( doesFileExist, getHomeDirectory, renameFile )

import CPM.Query.Configuration

--- Initial properties of the default RC template file.
defaultRCProps :: [Either String (String,String)]
defaultRCProps =
  [ Left "# Requests for classes, separated by comma:"
  , Left $ "# default: " ++ intercalate "," (defaultRequests Class)
  , Right ("classrequests", "")
  , Left ""
  , Left "# Requests for types, separated by comma:"
  , Left $ "# default: " ++ intercalate "," (defaultRequests Type)
  , Right ("typerequests", "")
  , Left ""
  , Left "# Requests for operations, separated by comma:"
  , Left $ "# default: " ++ intercalate "," (defaultRequests Operation)
  , Right ("operationrequests", "")
  , Left ""
  , Left "# Show all available information (no|yes):"
  , Right ("showall", "no")
  , Left ""
  , Left "# URL of the web installation of curry-info (used with option --cgi)"
  , Left "# (if empty: use default URL)"
  , Right ("curryinfocgi", "")
  ]

--- The contents of the default RC template file.
defaultRC :: String
defaultRC = unlines $
  map (either id (\ (k,v) -> k ++ "=" ++ v)) defaultRCProps

--- Location of the rc file of a user.
rcFileName :: IO FilePath
rcFileName = (</> ".cpmqueryrc") `fmap` getHomeDirectory

--- Reads the rc file. If it is not present, a new file will be created
--- with the contents of `defaultRC`.
readRC :: IO [(String, String)]
readRC = do
  rcname <- rcFileName
  rcexists  <- doesFileExist rcname
  catch (if rcexists
           then updateRC
           else do hPutStrLn stderr $ "Installing '" ++ rcname ++ "'..."
                   writeFile rcname defaultRC)
        (const $ return ())
  readPropertyFile rcname

--- Reads the rc file (which must be present) and compares the definitions
--- with the distribution rc file. If the set of variables is different,
--- update the rc file with the distribution but keep the user's definitions.
updateRC :: IO ()
updateRC = do
  rcname    <- rcFileName
  userprops <- readPropertyFile rcname
  let dfltprops = rights defaultRCProps
  unless (rcKeys userprops == rcKeys dfltprops) $ do
    hPutStrLn stderr $ "Updating '" ++ rcname ++ "'..."
    renameFile rcname $ rcname <.> "bak"
    writeFile rcname defaultRC
    mapM_ (\ (n, v) ->
             maybe (return ())
                   (\uv -> unless (uv == v) $ updatePropertyFile rcname n uv)
                   (lookup n userprops))
          dfltprops
 where
  rcKeys = sort . map fst

--- Look up a configuration variable in the list of variables from the rc file.
--- Uppercase/lowercase is ignored for the variable names and the empty
--- string is returned for an undefined variable.
rcValue :: [(String, String)] -> String -> String
rcValue rcdefs var = strip $ maybe "" id $
  lookup (map toLower var) (map (first (map toLower)) rcdefs)
 where
  first f (x, y) = (f x, y)

  strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------------
