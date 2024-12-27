------------------------------------------------------------------------
--- A tool to query analysis information about entities
--- (e.g., function, types, type classes) defined in a module
--- of some Curry package.
--- 
--- To use the tool to show information about some function, run
--- 
---     > cpm-query <module name> <function name>
---
--- Note that `cypm exec` is not necessary to invoke the tool since
--- the load path is computed by this tool. For instance, try
---
---     > cpm-query Data.List split
---     > cpm-query System.Process exitWith
---     > cpm-query System.Directory doesFileExist
---
--- @version December 2024
------------------------------------------------------------------------

module CPM.Query.Main
  ( main, askCurryInfoServer, askCurryInfoCmd)
 where

import Control.Monad      ( unless, when, replicateM, replicateM_ )
import Curry.Compiler.Distribution ( baseVersion )
import Data.Char          ( isDigit )
import Data.List          ( init, last, split )
import System.Environment ( getArgs, getEnv, setEnv )
import System.IO          ( hFlush, stderr, hClose, hGetLine, hPutStrLn )

import FlatCurry.Types    ( QName )
import Network.Socket     ( connectToSocket, close )
import System.CurryPath   ( lookupModuleSourceInLoadPath
                          , getPackageVersionOfDirectory
                          , getPackageVersionOfModule, setCurryPathIfNecessary
                          , sysLibPath )
import System.Directory   ( doesFileExist, getCurrentDirectory
                          , getModificationTime )
import System.FilePath    ( (</>), joinPath, splitDirectories )
import System.IOExts      ( evalCmd, execCmd, readCompleteFile )
import System.Path        ( fileInPath )
import System.Process     ( exitWith, system )

import CPM.Query.Configuration
import CPM.Query.Options

---------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "CPM Query Tool (Version of 23/12/24)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  checkExecutable
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
 where
  checkExecutable = do
    hascurryinfo <- fileInPath "curry-info"
    unless hascurryinfo $ do
      putStrLn $ "Binary 'curry-info' not found in PATH!\n" ++
        "Install it by the the following commands:\n\n" ++
        "> git clone https://github.com/curry-language/curry-info-system.git\n" ++
        "> cd curry-info-system\n" ++
        "> cypm install\n"
      exitWith 1

-- Start the tool to generate analysis information for a package:
generateForPackage:: Options -> String -> String -> IO ()
generateForPackage opts pkg vsn = do
  printWhenStatus opts $
    "Generating infos for package '" ++ pkg ++ "-" ++ vsn ++ "' for " ++
    show (optEntity opts) ++ " entities..."
  when (optEntity opts == Unknown) $ exitWith 0
  mods <- getPackageInfos opts pkg vsn ["modules"]
  mapM_ (generateForModule opts pkg vsn) mods

generateForModule :: Options -> String -> String -> String -> IO ()
generateForModule opts pkg vsn mn = do
  genInfo ""
  let cicmd = [ curryInfoVerb opts, "-f2", "-p", pkg, "-x"
              , escapeShellString vsn, "-m", mn]
  let tcreq = "--allclasses" : defaultRequests Class
  genInfo (unwords tcreq)
  runCommand opts $ unwords $ cicmd ++ tcreq
  let treq = "--alltypes": defaultRequests Type
  genInfo (unwords treq)
  runCommand opts $ unwords $ cicmd ++ treq
  ops <- getPackageInfos opts pkg vsn ["-m", mn, "operations"]
  case firstLocalOp ops of
    Nothing -> return ()
    Just op -> do
      mapM_ (\r -> do genInfo ("alloperations " ++ r)
                      runCommand opts $ unwords $
                        cicmd ++ ["--alloperations", r])
            ["signature", "definition"]
      let opreqs = defaultRequests Operation
      -- other operation analysis requests are only computed for the first local
      -- operation since this implicitly set the analysis results for all ops
      mapM_ (\r -> do genInfo ("alloperations " ++ r)
                      runCommand opts $ unwords $
                        cicmd ++ ["-o", escapeShellString op, r])
            opreqs
  where
   firstLocalOp ops = case filter (null . fst) (map fromQName ops) of
                        []  -> Nothing
                        x:_ -> Just (snd x)

   genInfo req = printWhenStatus opts $
     "Generating infos for module '" ++ mn ++
     (if null req then "" else "' and request '" ++ req) ++ "'..."

--- Computes some information of a package version by `curry-info`.
--- In case of a parse error, the program is terminated with an error state.
--- The final parameter is the list of paramters passed to `curry-info` after
--- the package and version options to specify a _single_ request.
--- For instance, these can be:
--- * To get all modules: `["modules"]`
--- * To get all operations in module `mn`: `["-m", mn, "operations"]`
getPackageInfos :: Read a => Options -> String -> String -> [String] -> IO a
getPackageInfos opts pkg vsn requests = do
  printWhenStatus opts $ "Generating infos for package '" ++ pkg ++ "-" ++ vsn
                         ++ "' for requests: " ++ unwords requests
  let cmdopts = ["-v0", "--format=CurryTerm", "-p", pkg, "-x", vsn] ++ requests
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
  infolist <- case reads infos of
                [(ms,_)] -> return (ms :: [(String,String)])
                _ -> putStrLn ("Parse error for: " ++ infos) >> exitWith 1
  -- finally, the (first) result string is a list of module names:
  let infostring = snd (head infolist)
  case reads infostring of
    [(ms,_)] -> return ms
    _        -> putStrLn ("Parse error for: " ++ infostring) >> exitWith 1

--- The binary name of the curry-info tool.
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

--- Transforms a possible qualified name into a pair of a module name
--- (which might be empty) and an unqualified name.
fromQName :: String -> (String,String)
fromQName = fromQN ""
 where
  fromQN mp s =
    let (m,dotn) = break (=='.') s
    in if null dotn || not (isModuleID m)
         then (mp,s)
         else if null mp then fromQN m (tail dotn)
                         else fromQN (mp ++ '.':m) (tail dotn)

  -- Is a string a (non-hierarchical) module identifier?
  isModuleID :: String -> Bool
  isModuleID []     = False
  isModuleID (x:xs) = isAlpha x && all (\c -> isAlphaNum c || c `elem` "'_") xs


------------------------------------------------------------------------------
-- Start the query tool:
startQueryTool :: Options -> String -> String -> IO ()
startQueryTool opts mname ename = do
  setCurryPathIfNecessary
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing -> error $ "Module '" ++ mname ++ "' not found!"
    Just (dirname,filename) -> do
      getPackageVersionOfDirectory dirname >>= maybe
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
           putStrLn edescr
           let request = if null (optRequest opts)
                           then defaultRequests (optEntity opts)
                           else optRequest opts
               icmd = unwords $
                        [ curryInfoVerb opts ] ++
                        [ "--format=" ++ optOutFormat opts ] ++
                        (if optForce opts && not (optShowAll opts)
                           then ["-f1"]
                           else ["-f0"]) ++
                        (if optShowAll opts then ["--showall"] else []) ++
                        [ "-p", pname, "-x", escapeShellString vers
                        , "-m", mname] ++ entityParam ++ request
           runCommand opts icmd
        )
 where
  entityParam = case optEntity opts of
                  Operation     -> [ "-o", escapeShellString ename ]
                  Type          -> [ "-t", escapeShellString ename ]
                  Class         -> [ "-c", escapeShellString ename ]
                  Unknown       -> []

-- Escape a string to use it in a shell command.
escapeShellString :: String -> String
escapeShellString s = '\'' : concatMap escapeSingleQuote s ++ "'"
 where
  escapeSingleQuote c | c == '\'' = "'\\\''"
                      | otherwise = [c]

------------------------------------------------------------------------------
--- This action starts `curry-info` in server mode and returns the result
--- of the given request (third argument) for all operations in the module
--- provided as the first argument. The requested result is returned in its
--- string representation for each entity in the module.
--- The second argument is the kind of entity to be queried.
--- If it is `Unknown`, `Nothing` is returned.
---
--- The package and version are determined using the Curry loadpath.
--- If something goes wrong, Nothing is returned.
askCurryInfoServer :: String -> CurryEntity -> String
                   -> IO (Maybe [(QName, String)])
askCurryInfoServer modname entkind req
  | entkind == Unknown = return Nothing
  | otherwise = do
    mres <- getPackageVersionOfModule modname
    case mres of
      Nothing -> return Nothing
      Just (pkg, vsn) -> do
        -- Note: force=0 is important to avoid loops if the analysis tools
        -- also use `curry-info`!
        (srvin, srvout, srverr) <- execCmd "curry-info --quiet -f0 --server &"

        -- Get port
        port <- fmap readPort (hGetLine srvout)
        hPutStrLn stderr $ "Connecting to port " ++ port

        -- Connect to server
        handle <- connectToSocket "localhost" (read port)

        -- Send requests
        let srvcmd = case entkind of
                       Type   -> "RequestAllTypesInformation"
                       Class  -> "RequestAllClassesInformation"
                       _      -> "RequestAllOperationsInformation"
        let msg = srvcmd ++ " curryterm 0 " ++ pkg ++ " " ++ vsn ++ " " ++
                  modname ++ " " ++ req
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
          _ -> do hPutStrLn stderr $ "Unexpected message: " ++ resultMsg
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
  readPort s = words s !! 2 -- First line of server: "Server Port: <n>"

  readResult :: (String, String) -> IO (QName, String)
  readResult (obj, res) = do
    let (m, o) = read obj :: (String, String)
        [(_, result)] = read res :: [(String, String)]
    return ((m, o), result)

------------------------------------------------------------------------------
--- This action uses the `curry-info` command to return the result
--- of the given request (third argument) for all operations in the module
--- provided as the first argument. The requested result is returned in its
--- string representation for each entity in the module.
--- The second argument is the kind of entity to be queried.
--- If it is `Unknown`, `Nothing` is returned.
--- 
--- The package and version are determined using the Curry loadpath.
--- If something goes wrong, Nothing is returned.
askCurryInfoCmd :: String -> CurryEntity -> String
               -> IO (Maybe [(QName, String)])
askCurryInfoCmd modname entkind req
  | entkind == Unknown = return Nothing
  | otherwise = do
    mres <- getPackageVersionOfModule modname
    case mres of
      Nothing -> return Nothing
      Just (pkg, vsn) -> do
        -- Note: force=0 is important to avoid loops if the analysis tools
        -- also use `curry-info`!
        let alloption  = case entkind of Type   -> "--alltypes"
                                         Class  -> "--allclasses"
                                         _      -> "--alloperations"
            cmdopts    = [ "--quiet", "-f0", "-p", pkg, "-x", vsn, "-m", modname
                         , alloption, "--format=CurryTerm", req]
        (ec, out, err) <- evalCmd "curry-info" cmdopts ""

        if ec > 0
          then do putStrLn "Execution error. Output:"
                  unless (null out) $ putStrLn out
                  unless (null err) $ putStrLn err
                  return Nothing
          else do let results = read out :: [(String, String)]
                  fmap Just (mapM readResult results)
 where
  readResult :: (String, String) -> IO (QName, String)
  readResult (obj, res) = do
    let (m, o) = read obj :: (String, String)
        [(_, result)] = read res :: [(String, String)]
    return ((m, o), result)

----------------------------------------------------------------------------
