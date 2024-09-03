------------------------------------------------------------------------
--- A simple program to show analysis information about a function defined
--- in a module of some package.
--- 
--- To start the tool:
--- 
---     > curry-funcinfo <module name> <function name>
---
--- For instance, try
---
---     > curry-funcinfo Data.List split
---     > cypm exec curry-funcinfo System.Process exitWith
---     > cypm exec curry-funcinfo System.Directory doesFileExist
---
--- @version September 2024
------------------------------------------------------------------------

module Main where

import Control.Monad      ( when )
import Curry.Compiler.Distribution ( baseVersion )
import Data.Char          ( isDigit, toLower )
import Data.List          ( init, isPrefixOf, last, split )
import System.Environment ( getArgs )
import System.IO          ( hFlush, stdout )

import System.CurryPath   ( lookupModuleSourceInLoadPath, sysLibPath )
import System.Directory   ( doesFileExist )
import System.FilePath    ( joinPath, splitDirectories )
import System.Process     ( exitWith, system )

---------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [mn,fn] -> startInfoTool mn fn
    _       -> do putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n\n" ++
                             usageText
                  exitWith 1

usageText :: String
usageText = "Usage: curry-funcinfo <module name> <function name>"

-- Start the information tool:
startInfoTool :: String -> String -> IO ()
startInfoTool mname fname = do
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing -> error $ "Module '" ++ mname ++ "' not found!"
    Just (dirname,filename) -> do
      --putStrLn $ "DIR : " ++ dirname
      getPackageId dirname >>= maybe
        (putStrLn $
           "Module '" ++ mname ++ "' stored in file:\n" ++
           filename ++
           "\nbut no registered CPM package found having source directory:\n" ++
           dirname)
        (\(pname,vers) -> do
           -- do something with package, version, module, and function:
           putStrLn $ "Package name   : " ++ pname
           putStrLn $ "Package version: " ++ vers
           putStrLn $ "Module name    : " ++ mname
           putStrLn $ "Function name  : " ++ fname
           let icmd = infoCmd pname vers 0
           putStrLn $ "Executing: " ++ icmd
           system icmd
           ans <- askYesNo "Force computation of all properties? (No|yes) "
           when (ans == "yes") $ do
             let icmd = infoCmd pname vers 1
             putStrLn $ "Executing: " ++ icmd
             system icmd >> return ()
        )
 where
  infoCmd pname vers f =
    unwords ["curry-info", "-f" ++ show f,
             "-p", pname, "-x", enclose vers,
             "-m", mname, "-o", enclose fname,
             "signature totallyDefined deterministic termination"]

  enclose s = '"' : s ++ "\""

-- Check whether a file path (a list of directory names) is part of a
-- package and return the package name and package version.
-- For instance,
--
--     getPackageId "/home/joe/mytool/.cpm/packages/process-3.0.0/src"
--
-- returns
--
--     Just "process" "3.0.0"
--
-- For this purpose, it is checked whether there is a `package.json` file
-- under the directory and the directory name is a valid package id.
getPackageId :: String -> IO (Maybe (String,String))
getPackageId path =
  if sysLibPath == [path]
    then return (Just ("base",baseVersion))
    else  checkPackageId (splitDirectories path)
 where
  checkPackageId []             = return Nothing
  checkPackageId dirnames@(_:_) = do
    expkg <- doesFileExist (joinPath (dirnames ++ ["package.json"]))
    if expkg
      then return (splitPkgId "" (last dirnames))
      else checkPackageId (init dirnames)

  splitPkgId oldpn s =
    let (pname,hvers) = break (=='-') s
        newpn = if null oldpn then pname else oldpn ++ "-" ++ pname
    in if null hvers
         then Nothing
         else let vers = tail hvers
              in if isVersionId vers then Just (newpn,vers)
                                     else splitPkgId newpn vers

  isVersionId vs = case split (=='.') vs of
    (maj:min:patch:_) -> all (all isDigit) [maj, min, take 1 patch]
    _                 -> False

----------------------------------------------------------------------------
-- Axuiliaries:

-- Ask a question and return the answer which must be empty, `yes`, or `no`.
askYesNo :: String -> IO String
askYesNo question = do
  putStr question
  hFlush stdout
  answer <- fmap (map toLower) getLine
  if null answer
    then return answer
    else if answer `isPrefixOf` "yes"
           then return "yes"
           else if answer `isPrefixOf` "no"
                  then return "no"
                  else askYesNo question -- again

----------------------------------------------------------------------------