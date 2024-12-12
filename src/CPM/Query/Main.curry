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
--- @version December 2024
------------------------------------------------------------------------

module CPM.Query.Main (main, askCurryInfoServer) where

import Control.Monad      ( unless, when, replicateM, replicateM_ )
import Curry.Compiler.Distribution ( baseVersion )
import Data.Char          ( isDigit )
import Data.List          ( init, last, split )
import System.Environment ( getArgs, getEnv, setEnv )
import System.IO          ( hFlush, stderr, hClose, hGetLine, hPutStrLn )

import System.CurryPath   ( lookupModuleSourceInLoadPath, setCurryPath
                          , sysLibPath )
import System.Directory   ( doesFileExist, getCurrentDirectory
                          , getModificationTime )
import System.FilePath    ( (</>), joinPath, splitDirectories )
import System.Process     ( exitWith, system )

import System.IOExts      ( evalCmd, execCmd, readCompleteFile )

import FlatCurry.Types (QName)

import Network.Socket (connectToSocket, close)

import CPM.Query.Options

---------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "CPM Query Tool (Version of 12/12/24)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  (opts,args) <- getArgs >>= processOptions banner
  when (optEntity opts == Unknown) $ do
    printWhenStatus opts $
      "No information for entity of kind '" ++ optCLS opts ++ "'"
    exitWith 0
  case args of
    [pkg,vsn,mn] | optGenerate opts -> generateForModule opts pkg vsn mn
    [mn,fn] -> if optGenerate opts then generateForPackage opts mn fn 
                                   else startQueryTool opts mn fn
    _       -> do putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n\n" ++
                             usageText
                  exitWith 1

-- Start the tool to generate analysis information for a package:
generateForPackage:: Options -> String -> String -> IO ()
generateForPackage opts pkg vsn = do
  printWhenStatus opts $
    "Generating information for package '" ++ pkg ++ "-" ++ vsn ++ "' for " ++
    show (optEntity opts) ++ " entities..."
  when (optEntity opts == Unknown) $ exitWith 0
  mods <- getPackageModules opts pkg vsn
  mapM_ (generateForModule opts pkg vsn) mods

generateForModule :: Options -> String -> String -> String -> IO ()
generateForModule opts pkg vsn mn = do
  printWhenStatus opts $ "Generating analysis data for module '" ++ mn ++ "'..."
  let cicmd = [curryInfoVerb opts, "-f2", "-p", pkg, "-x", enclose vsn, "-m", mn]
  runCommand opts $ unwords $ cicmd ++ ["--alltypeclasses"]
  runCommand opts $ unwords $ cicmd ++ ["--alltypes"]
  ops <- getModuleOperations opts pkg vsn mn
  unless (null ops) $ do
    let opreqs = defaultRequest (opts { optEntity = Operation })
    mapM_ (\r -> runCommand opts $ unwords $
                   cicmd ++ ["-o", enclose (head ops), r])
          (opreqs)

-- Compute the modules of a package by `curry-info`:
getPackageModules :: Options -> String -> String -> IO [String]
getPackageModules opts pkg vsn = do
  printWhenStatus opts $ "Generating information for package '" ++
                         pkg ++ "-" ++ vsn ++ "'..."
  let cmdopts = [ "-v" ++ show (optVerb opts)
                , "--output=CurryTerm", "-p", pkg, "-x", vsn
                , "modules"]
  printWhenAll opts $ unwords $ ["Executing:", curryInfoBin] ++ cmdopts
  (ec,sout,serr) <- evalCmd curryInfoBin cmdopts ""
  printWhenAll opts $ "Exit code: " ++ show ec ++ "\nSTDOUT:\n" ++ sout ++
                      "\nSTDERR:\n" ++ serr
  when (ec > 0) $ putStrLn "ERROR OCCURRED" >> exitWith 1
  -- Now we parse the output of curry-info:
  -- curry-info returns a list of strings pairs (package string, info string):
  resterm <- case reads sout of
               [(ms,_)] -> return (ms :: [(String,String)])
               _ -> putStrLn ("Parse error for: " ++ sout) >> exitWith 1
  -- the info string is a pair of requests and results as strings:
  let infos = snd (head resterm)
  infoterm <- case reads infos of
               [(ms,_)] -> return (ms :: [(String,String)])
               _ -> putStrLn ("Parse error for: " ++ infos) >> exitWith 1
  -- finally, the (first) result string is a list of module names:
  let mods = snd (head infoterm)
  modules <- case reads mods of
               [(ms,_)] -> return (ms :: [String])
               _ -> putStrLn ("Parse error for: " ++ mods) >> exitWith 1
  printWhenStatus opts $ "Modules: " ++ unwords modules
  return modules

-- Compute the operations of a module of a package by `curry-info`:
getModuleOperations :: Options -> String -> String -> String -> IO [String]
getModuleOperations opts pkg vsn mn = do
  printWhenStatus opts $ "Generating information for package '" ++
                         pkg ++ "-" ++ vsn ++ "'..."
  let cmdopts = [ "-v" ++ show (optVerb opts)
                , "--output=CurryTerm", "-p", pkg, "-x", vsn, "-m", mn
                , "operations"]
  printWhenAll opts $ unwords $ ["Executing:", curryInfoBin] ++ cmdopts
  (ec,sout,serr) <- evalCmd curryInfoBin cmdopts ""
  printWhenAll opts $ "Exit code: " ++ show ec ++ "\nSTDOUT:\n" ++ sout ++
                      "\nSTDERR:\n" ++ serr
  when (ec > 0) $ putStrLn "ERROR OCCURRED" >> exitWith 1
  -- Now we parse the output of curry-info:
  -- curry-info returns a list of strings pairs (package string, info string):
  resterm <- case reads sout of
               [(ms,_)] -> return (ms :: [(String,String)])
               _ -> putStrLn ("Parse error for: " ++ sout) >> exitWith 1
  -- the info string is a pair of requests and results as strings:
  let infos = snd (head resterm)
  infoterm <- case reads infos of
               [(ms,_)] -> return (ms :: [(String,String)])
               _ -> putStrLn ("Parse error for: " ++ infos) >> exitWith 1
  -- finally, the (first) result string is a list of module names:
  let mods = snd (head infoterm)
  operations <- case reads mods of
                  [(ms,_)] -> return (ms :: [String])
                  _ -> putStrLn ("Parse error for: " ++ mods) >> exitWith 1
  printWhenStatus opts $ "Operations of module '" ++ mn ++ "':\n" ++
                         unwords operations
  return operations

-- The binary name of the curry-info tool.
curryInfoBin :: String
curryInfoBin = "curry-info"

-- The binary name of the curry-info tool together with verbosity option.
curryInfoVerb :: Options -> String
curryInfoVerb opts = unwords [curryInfoBin, "-v" ++ show (optVerb opts)]

-- Show and run a command (if not in dry-run mode).
runCommand :: Options -> String -> IO ()
runCommand opts cmd = do
  when (optVerb opts > 2 || optDryRun opts) $ putStrLn $ "Executing: " ++ cmd
  unless (optDryRun opts) $ system cmd >> return ()

------------------------------------------------------------------------------
-- Start the query tool:
startQueryTool :: Options -> String -> String -> IO ()
startQueryTool opts mname ename = do
  setCurryPathIfNecessary
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing -> error $ "Module '" ++ mname ++ "' not found!"
    Just (dirname,filename) -> do
      getPackageId dirname >>= maybe
        (printWhenStatus opts $
           "Module '" ++ mname ++ "' " ++
           (if optVerb opts > 1
              then  "stored in file\n  " ++ filename ++
                    "\nbut this does not belong to the sources of a "
              else "does not belong to a ") ++
           "registered CPM package!")
        (\(pname,vers) -> do
           -- do something with package, version, module, and function:
           let edescr = if optVerb opts > 1
                          then unlines [ "Package name   : " ++ pname
                                       , "Package version: " ++ vers
                                       , "Module name    : " ++ mname
                                       , "Entity name    : " ++ ename ]
                          else show (optEntity opts) ++ " " ++
                               mname ++ "." ++ ename ++
                               " (package " ++ pname ++ "-" ++ vers ++ ")"
           printWhenStatus opts $ edescr
           let request = if null (optRequest opts) then defaultRequest opts
                                                   else optRequest opts
               icmd = unwords $
                         [ curryInfoVerb opts ] ++
                         [ "--output=" ++ optOutFormat opts ] ++
                         (if optForce opts then ["-f1"] else ["-f0"]) ++
                         [ "-p", pname, "-x", enclose vers
                         , "-m", mname] ++ entityParam ++ request
           runCommand opts icmd
        )
 where
  entityParam = case optEntity opts of
                  Operation     -> [ "-o", enclose ename ]
                  Type          -> [ "-t", enclose ename ]
                  TypeClass     -> [ "-c", enclose ename ]
                  Unknown       -> []

enclose :: String -> String
enclose s = '"' : concatMap escapeBackslash s ++ "\""
 where
  escapeBackslash c | c == '\\' = "\\\\"
                    | otherwise = [c]

-- The default requests for various kinds entities.
defaultRequest :: Options -> [String]
defaultRequest opts = case optEntity opts of
  Operation     -> [ "cass-deterministic", "cass-total"
                   , "cass-terminating", "cass-demand", "failfree" ]
  Type          -> [ "definition" ]
  TypeClass     -> [ "definition" ]
  Unknown       -> []

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
    else getPackageSpecPath (splitDirectories path) >>=
         maybe (return Nothing)
               (\dirnames -> return (splitPkgId "" (last dirnames)))
 where
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

--- Returns, for a given directory path, the directory path containing
--- a package specification.
getPackageSpecPath :: [String] -> IO (Maybe [String])
getPackageSpecPath []             = return Nothing
getPackageSpecPath dirnames@(_:_) = do
  expkg <- doesFileExist (joinPath (dirnames ++ [packageSpecFile]))
  if expkg
    then return (Just dirnames)
    else getPackageSpecPath (init dirnames)

--- If the environment variable `CURRYPATH` is not already set
--- (i.e., not null), set it to the value stored in CPM's `CURRYPATH_CACHE`
--- file or set it by `System.CurryPath.setCurryPath`
--- (which uses `cypm deps --path` to compute its value).
setCurryPathIfNecessary :: IO ()
setCurryPathIfNecessary = do
  cp <- getEnv "CURRYPATH"
  when (null cp) $ do
    cdir <- getCurrentDirectory
    getPackageSpecPath (splitDirectories cdir) >>=
      maybe setCurryPathByCPM (loadCurryPathFromCache . joinPath)
 where
  loadCurryPathFromCache specdir = do
    let cachefile = specdir </> ".cpm" </> "CURRYPATH_CACHE"
    excache <- doesFileExist cachefile
    if excache
      then do
        cftime <- getModificationTime cachefile
        pftime <- getModificationTime (specdir </> packageSpecFile)
        if cftime > pftime
          then do cnt <- readCompleteFile cachefile
                  let cpath = head (lines cnt)
                  setEnv "CURRYPATH" cpath
          else setCurryPathByCPM
      else setCurryPathByCPM

  setCurryPathByCPM = setCurryPath True ""

  --- The name of the package specification file in JSON format.
packageSpecFile :: String
packageSpecFile = "package.json"

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
            (srvin, srvout, srverr) <- execCmd "curry-info --server &"

            -- Get port
            hGetLine srvout
            portLine <- hGetLine srvout
            let port = readPort portLine
            hPutStrLn stderr port

            -- Connect to server
            handle <- connectToSocket "localhost" (read port)

            -- Send requests
            let msg = "RequestAllOperationsInformation curryterm 0 " ++ pkg ++
                      " " ++ vsn ++ " " ++ modname ++ " " ++ req
            hPutStrLn handle msg
            hFlush handle
            resultMsg <- hGetLine handle

            result <- case words resultMsg of
              ["ok", numberOfLines] -> do
                ls <- replicateM (read numberOfLines) (hGetLine handle)
                let results = read (unlines ls) :: [(String, String)]
                fmap Just (mapM readResult results)

              ("error":errmsg) -> do
                hPutStrLn stderr $ "Error message: " ++ unwords errmsg
                return Nothing

              _ -> do
                hPutStrLn stderr $ "Unexpected message: " ++ resultMsg
                return Nothing
            
            -- Shut down server
            hPutStrLn handle "StopServer"
            hFlush handle
              
            -- Close handles
            hClose srvin
            hClose srvout
            hClose srverr

            return result

            
  where
    readPort s = words s !! 2

    readResult :: (String, String) -> IO (QName, String)
    readResult (obj, res) = do
      let (_, _, m, o) = read obj :: (String, String, String, String)
      let [(_, result)] = read res :: [(String, String)]
      return ((m, o), result)

----------------------------------------------------------------------------
