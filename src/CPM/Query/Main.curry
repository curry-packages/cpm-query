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
--- @version October 2024
------------------------------------------------------------------------

module CPM.Query.Main (main, askCurryInfoServer) where

import Control.Monad      ( when, replicateM, replicateM_ )
import Curry.Compiler.Distribution ( baseVersion )
import Data.Char          ( isDigit )
import Data.List          ( init, last, split )
import System.Environment ( getArgs )
import System.IO          ( hFlush, stderr, hClose, hGetLine, hPutStrLn )

import System.CurryPath   ( lookupModuleSourceInLoadPath, sysLibPath )
import System.Directory   ( doesFileExist )
import System.FilePath    ( joinPath, splitDirectories )
import System.Process     ( exitWith, system )

import System.IOExts (execCmd)

import FlatCurry.Types (QName)

import CPM.Query.Options

---------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "CPM Query Tool (Version of 19/09/24)"
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
           let request = if null (optRequest opts) then dfltRequest
                                                   else [optRequest opts]
               icmd = unwords $
                         [ "curry-info", "-v" ++ show (optVerb opts) ] ++
                         [ "--output=" ++ optOutFormat opts ] ++
                         (if optForce opts then ["-f1"] else ["-f0"]) ++
                         [ "-p", pname, "-x", enclose vers
                         , "-m", mname] ++ entityParam ++ request
           printWhenAll opts $ "Executing: " ++ icmd
           system icmd >> return ()
        )
 where
  entityParam = case optEntity opts of
                  Operation -> [ "-o", enclose fname ]
                  Type      -> [ "-t", enclose fname ]
                  TypeClass -> [ "-c", enclose fname ]

  -- the default requests for various entities:
  dfltRequest = case optEntity opts of
                  Operation -> [ "signature deterministic"
                               , "totallyDefined termination demandness" ]
                  Type      -> [ "definition" ]
                  TypeClass -> [ "definition" ]

  enclose s = '"' :  concatMap escapeBackslash s ++ "\""

  escapeBackslash c | c == '\\' = "\\\\"
                    | otherwise = [c]

--- Checks whether a module name is part of a package and
--- returns the package name and package version.
--- For instance, in a package containing a dependency to package
--- `process` with version `3.0.0`, the call
---
---     getPackageIdOfModule "System.Process"
---
--- returns
---
---     Just "process" "3.0.0"
---
--- `Nothing` is returned if there is no package to which this module
--- belongs.
---
--- For this purpose, the source file of the module is looked up
--- (and an error is raised if this module cannot be found) and
--- it is checked whether there is a `package.json` file under the
--- directory of the source file and the directory name is a valid package id.
getPackageIdOfModule :: String -> IO (Maybe (String,String))
getPackageIdOfModule mname = do
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing -> error $ "Module '" ++ mname ++ "' not found in load path!"
    Just (dirname,_) -> getPackageId dirname

--- Checks whether a file path (a list of directory names) is part of a
--- package and returns the package name and package version.
--- For instance,
---
---     getPackageId "/home/joe/mytool/.cpm/packages/process-3.0.0/src"
---
--- returns
---
---     Just "process" "3.0.0"
---
--- For this purpose, it is checked whether there is a `package.json` file
--- under the directory and the directory name is a valid package id.
getPackageId :: String -> IO (Maybe (String,String))
getPackageId path =
  if sysLibPath == [path]
    then return (Just ("base",baseVersion))
    else checkPackageId (splitDirectories path)
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

--- This action starts curry-info in server mode and returns the result of the
--- given request for the given module.
--- The package and version and determined using the loadpath.
--- If something goes wrong, Nothing is returned.
askCurryInfoServer :: String -> String -> IO (Maybe [(QName, String)])
askCurryInfoServer modname req = do
    mres <- getPackageIdOfModule modname
    case mres of
        Nothing -> return Nothing
        Just (pkg, vsn) -> do
            (serverInput, serverOutput, serverErr) <- execCmd "curry-info --server &"

            -- Get port
            hGetLine serverOutput
            portLine <- hGetLine serverOutput
            let port = readPort portLine
            hPutStrLn stderr port

            -- Connect to server
            (telnetInput, telnetOutput, telnetErr) <- execCmd $ "telnet localhost " ++ port

            -- Skip first 4 lines of output
            replicateM_ 4 (hGetLine telnetOutput)

            -- Send requests
            let msg = "RequestAllOperationsInformation curryterm 0 " ++ pkg ++ " " ++ vsn ++ " " ++ modname ++ " " ++ req
            hPutStrLn telnetInput msg
            hFlush telnetInput
            resultMsg <- hGetLine telnetOutput

            -- Shut down server
            hPutStrLn telnetInput "StopServer"
            hFlush telnetInput

            result <- case words resultMsg of
              ["ok", numberOfLines] -> do
                ls <- replicateM (read numberOfLines) (hGetLine telnetOutput)
                let results = read (unlines ls) :: [(String, String)]
                fmap Just (mapM readResult results)

              ("error":errmsg) -> do
                hPutStrLn stderr $ "Error message: " ++ unwords errmsg
                return Nothing

              _ -> do
                hPutStrLn stderr $ "Unexpected message: " ++ resultMsg
                return Nothing
              
            -- Close handles
            hClose serverInput
            hClose serverOutput
            hClose serverErr

            hClose telnetInput
            hClose telnetOutput
            hClose telnetErr

            return result

            
  where
    readPort s = words s !! 2

    readResult :: (String, String) -> IO (QName, String)
    readResult (obj, res) = do
      let (_, _, m, o) = read obj :: (String, String, String, String)
      let [(_, result)] = read res :: [(String, String)]
      return ((m, o), result)

----------------------------------------------------------------------------
