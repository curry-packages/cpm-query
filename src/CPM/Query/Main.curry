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
--- @version January 2025
------------------------------------------------------------------------

module CPM.Query.Main
  ( main, askCurryInfoServer, askCurryInfoCmd)
 where

import Control.Monad      ( unless, when, replicateM, replicateM_ )
import Curry.Compiler.Distribution ( baseVersion )
import Data.Char          ( isDigit )
import Data.List          ( init, intercalate, isPrefixOf, last, split )
import System.Environment ( getArgs, getEnv, setEnv )
import System.IO          ( getContents, hFlush, stderr, hClose, hGetLine
                          , hPutStrLn )

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
  bannerText = "CPM Query Tool (Version of 06/01/25)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  checkExecutable
  (opts,args) <- getArgs >>= processOptions banner
  when (optEntity opts == Unknown) $ do
    printWhenStatus opts $
      "No information for entity of kind '" ++ optCLS opts ++ "'"
    exitWith 0
  let genfile = optGenFrom opts
  case args of
    [pkg,vsn,mn] | optGenerate opts -> generateForModule opts pkg vsn mn
    [mn,fn] -> if optGenerate opts then generateForPackage opts mn fn 
                                   else queryModuleEntity opts mn fn
    _       ->
      if null args && optGenerate opts && not (null genfile)
        then do ls <- if genfile == "-" then getContents else readFile genfile
                mapM_ (genFromLine opts) (lines ls)
        else do putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n\n" ++
                             usageText
                exitWith 1
 where
  genFromLine opts l = case words l of
    [p,v]   -> generateForPackage opts p v
    [p,v,m] -> generateForModule opts p v m
    _       -> error $ "Illegal line in generate file: " ++ l

  checkExecutable = do
    hascurryinfo <- fileInPath "curry-info"
    unless hascurryinfo $ do
      putStrLn $ "Binary 'curry-info' not found in PATH!\n" ++
        "Install it by the the following commands:\n\n" ++
        "> git clone https://github.com/curry-language/curry-info-system.git\n" ++
        "> cd curry-info-system\n" ++
        "> cypm install\n"
      exitWith 1

------------------------------------------------------------------------------
-- Generate analysis information for a given package and version.
-- In CGI mode, the package repository index is also updated.
generateForPackage:: Options -> String -> String -> IO ()
generateForPackage opts pkg vsn = do
  printWhenStatus opts $
    "Generating infos for package '" ++ pkg ++ "-" ++ vsn ++ "' for " ++
    show (optEntity opts) ++ " entities..."
  when (optEntity opts == Unknown) $ exitWith 0
  when (optCGI opts) $ do  -- update repo index in CGI mode:
    printWhenStatus opts "Update package repository index..."
    callCurryInfo opts [ "--update" ]
  mods <- getPackageInfos opts pkg vsn Nothing
  mapM_ (generateForModule opts pkg vsn) mods

-- Generate analysis information for a given package, version, and module.
generateForModule :: Options -> String -> String -> String -> IO ()
generateForModule opts pkg vsn mn = do
  genInfo []
  let modopts = [ "--package=" ++ escapeString opts pkg
                , "--version=" ++ escapeString opts vsn
                , "--module="  ++ escapeString opts mn ]
  when (null (optRequest opts)) $ do
    printWhenStatus opts $ "Cleaning information of module '" ++ mn ++ "'..."
    callCurryInfo opts (modopts ++ ["--clean"])
  let ciopts  = "--force=2" : modopts
      optreqs = optRequest opts
  if null optreqs
    then do
      infoAndCallCurry ciopts ("--allclasses" : defaultRequests Class)
      infoAndCallCurry ciopts ("--alltypes" : defaultRequests Type)
      mapM_ (genOpRequest ciopts)
            (["signature", "definition"] ++ defaultRequests Operation)
    else case optEntity opts of
      Class     -> infoAndCallCurry ciopts ("--allclasses"    : optreqs)
      Type      -> infoAndCallCurry ciopts ("--alltypes"      : optreqs)
      Operation -> mapM_ (genOpRequest ciopts) optreqs
      Unknown   -> return ()
 where
  -- is the request one which generates infos for all operations at once?
  singleOpRequest req = "cass-" `isPrefixOf` req || req == "failfree"

  genOpRequest ciopts req
    | singleOpRequest req
    = fmap firstLocalOp (getPackageInfos opts pkg vsn (Just mn)) >>=
      maybe (return ())
        (\op ->
          -- compute request only for the first local operation since
          -- this implicitly sets the analysis results for all operations:
          infoAndCallCurry ciopts ["--operation=" ++ escapeString opts op, req])
    | otherwise
    = infoAndCallCurry ciopts ["--alloperations", req]
    
  infoAndCallCurry ciopts reqs = do
    genInfo reqs
    callCurryInfo opts $ ciopts ++ reqs

  firstLocalOp ops = case filter (null . fst) (map fromQName ops) of
                       []  -> Nothing
                       x:_ -> Just (snd x)

  genInfo req = printWhenStatus opts $
    "Generating infos for '" ++ mn ++
    (if null req then "" else "' and '" ++ unwords req) ++ "'..."

--- Computes some information of a package version by `curry-info`.
--- In case of a parse error, the program is terminated with an error state.
--- The final parameter is kind of request, which is `Nothing` to return
--- all modules or `Just m` to return all operations of module `m`.
getPackageInfos :: Read a => Options -> String -> String -> Maybe String -> IO a
getPackageInfos opts pkg vsn mbmod = do
  let requests = maybe ["modules"]
                       (\m -> ["--module=" ++ m, "operations"])
                       mbmod
  printWhenStatus opts $ "Generating infos for '" ++ pkg ++ "-" ++ vsn
                         ++ "' and '" ++ unwords requests ++ "'..."
  let cmdopts = [ "-v0", "--format=CurryTerm", "--package=" ++ pkg
                , "--version=" ++ vsn] ++ requests
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
  case resterm of
    [] -> putStrLn "NO INFORMATION FOUND!" >> exitWith 1
    ((_,infos):_) -> do
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
curryInfoVerb opts =
  unwords [curryInfoBin, "--verbosity=" ++ show (optVerb opts)]

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
-- Query an entity of a given module. The module is looked up in the
-- current load path of Curry.
queryModuleEntity :: Options -> String -> String -> IO ()
queryModuleEntity opts mname ename = do
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
               ciopts = [ "--format=" ++ optOutFormat opts ] ++
                        (if optForce opts && not (optShowAll opts)
                           then ["-f1"]
                           else ["-f0"]) ++
                        (if optShowAll opts then ["--showall"] else []) ++
                        [ "--package=" ++ escapeString opts pname
                        , "--version=" ++ escapeString opts vers
                        , "--module="  ++ escapeString opts mname] ++
                        entityParam ++ request
           callCurryInfo opts ciopts
        )
 where
  escename = escapeString opts ename

  entityParam = case optEntity opts of
                  Operation     -> [ "--operation=" ++ escename ]
                  Type          -> [ "--type="      ++ escename ]
                  Class         -> [ "--class="     ++ escename ]
                  Unknown       -> []

-- Escape a string for shell comand or for CGI URL.
escapeString :: Options -> String -> String
escapeString opts s = if optCGI opts then string2urlencoded s
                                     else escapeShellString s

callCurryInfo :: Options -> [String] -> IO ()
callCurryInfo opts ciopts = do
  let cmd = if optCGI opts
              then "curl --max-time 3600 --silent --show-error '" ++
                   optCGIURL opts ++ "?" ++ intercalate "&" ciopts ++ "'"
              else unwords (curryInfoVerb opts : ciopts)
  runCommand opts cmd

-- From HTML.Base:
-- Translates arbitrary strings into equivalent URL encoded strings.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | isAlphaNum c = c : string2urlencoded cs
  | c == ' '     = '+' : string2urlencoded cs
  | otherwise
  = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
  int2hex i = if i<10 then chr (ord '0' + i)
                      else chr (ord 'A' + i - 10)

-- Escape a string to use it in a shell command.
escapeShellString :: String -> String
escapeShellString s
  | all isAlphaNum s = s
  | otherwise        = '\'' : concatMap escapeSingleQuote s ++ "'"
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
--- of the given request (third argument) for all entities in the module
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
