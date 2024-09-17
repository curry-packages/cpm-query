------------------------------------------------------------------------
--- A tool to query analysis information about entities
--- (e.g., function, types, type classes) defined in a module
--- of some Curry package.
--- 
--- To use the tool to show information about some function, run
--- 
---     > cpm-query <module name> <function name>
---
--- For instance, try
---
---     > cpm-query Data.List split
---     > cypm exec cpm-query System.Process exitWith
---     > cypm exec cpm-query System.Directory doesFileExist
---
--- @version September 2024
------------------------------------------------------------------------

module CPM.Query.Main where

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

import CPM.Query.Options

---------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "CPM Query Tool (Version of 09/09/24)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  (opts,args) <- getArgs >>= processOptions banner
  case args of
    [mn,fn] -> startQueryTool opts mn fn
    _       -> do putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n\n" ++
                             usageText
                  exitWith 1

-- Start the query tool:
startQueryTool :: Options -> String -> String -> IO ()
startQueryTool opts mname fname = do
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing -> error $ "Module '" ++ mname ++ "' not found!"
    Just (dirname,filename) -> do
      --putStrLn $ "DIR : " ++ dirname
      getPackageId dirname >>= maybe
        (putStrLn $
           "Module '" ++ mname ++ "' stored in file\n  " ++
           filename ++ "\nbut this does not belong to the sources " ++
           "of a registered CPM package!")
        (\(pname,vers) -> do
           -- do something with package, version, module, and function:
           printWhenIntermediate opts $ unlines
              [ "Package name   : " ++ pname
              , "Package version: " ++ vers
              , "Module name    : " ++ mname
              , "Entity name    : " ++ fname
              ]
           let query = case optEntity opts of
                 Operation -> [ "-o", enclose fname
                              , "signature demandness deterministic"
                              , "totallyDefined termination" ]
                 Type      -> [ "-t", enclose fname
                              , "definition" ]
                 TypeClass -> [ "-c", enclose fname
                              , "definition" ]
               icmd = unwords $
                         [ "curry-info" ] ++
                         (if optJSON opts then ["--output=json"] else []) ++
                         (if optForce opts then ["-f1"] else ["-f0"]) ++
                         [ "-p", pname, "-x", enclose vers
                         , "-m", mname] ++ query
           printWhenAll opts $ "Executing: " ++ icmd
           system icmd >> return ()
        )
 where
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
