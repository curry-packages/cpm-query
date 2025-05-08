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
--- @version May 2025
------------------------------------------------------------------------

module CPM.Query.Main
  ( main, askCurryInfoServer, askCurryInfoCmd
  , getPackageModules, getPackageModuleOps )
 where

import Control.Monad      ( unless, when, replicateM, void )
import Data.List          ( intercalate, nubBy )
import System.Environment ( getArgs )
import System.IO          ( getContents, hFlush, stderr, stdout, hClose
                          , hGetLine, hPutStr, hPutStrLn )

import FlatCurry.Types    ( QName )
import Network.Socket     ( connectToSocket )
import Network.URL        ( params2urlencoded, string2urlencoded )
import System.CurryPath   ( lookupModuleSourceInLoadPath
                          , getPackageVersionOfDirectory
                          , getPackageVersionOfModule, setCurryPathIfNecessary
                          , sysLibPath )
import System.Directory   ( doesFileExist, getCurrentDirectory
                          , getModificationTime )
import System.FilePath    ( (</>), joinPath, splitDirectories, replaceDirectory )
import System.IOExts      ( evalCmd, execCmd, readCompleteFile )
import System.Path        ( fileInPath )
import System.Process     ( exitWith, system )
import Text.CSV           ( readCSV, showCSV )

import CPM.Query.Configuration
import CPM.Query.Options

---------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "CPM Query Tool (Version of 08/05/25)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  (opts,args) <- getArgs >>= processOptions banner
  unless (optRemote opts) checkExecutable
  when (optEntity opts == Unknown) $ do
    printWhenStatus opts $
      "No information for this kind of entity (" ++ optCLS opts ++ ")"
    exitWith 0
  let genfile = optGenFrom opts
  case args of
    [pkg,vsn,mn] | optGenerate opts -> generateForModule opts pkg vsn mn
    [mn,fn] -> if optGenerate opts then generateForPackage opts mn fn 
                                   else queryModuleEntity opts mn fn
    [mn]    -> queryModuleEntity opts mn ""
    [] | not (null (optModule opts))
            -> queryModuleEntity opts (optModule opts) ""
       | not (null (optPackage opts))
            -> queryPackage opts
       | optGenerate opts && not (null genfile)
            -> do table <- readCSV <$> if genfile == "-" then getContents
                                                         else readFile genfile
                  mapM_ (genFromFields opts) (nubBy eqPkgVrs table)
    _       -> do putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n\n" ++
                             usageText
                  exitWith 1
 where
  eqPkgVrs l1 l2 = case (l1,l2) of (p1:v1:_, p2:v2:_) -> p1==p2 && v1==v2
                                   _                  -> False
  genFromFields opts fs = case fs of
    (p:v:_) | isPackageName p -> generateForPackage opts p v
    []      -> return () -- skip empty lines 
    _       -> hPutStr stderr $
                 "*** Ignore illegal line in generate file: " ++ showCSV [fs]

  checkExecutable = do
    hascurryinfo <- fileInPath "curry-info"
    unless hascurryinfo $ do
      putStrLn $ "Binary 'curry-info' not found in PATH!\n" ++
        "Use 'curry-info' web service (option '--remote') or\n" ++
        "install it by the the following commands:\n\n" ++
        "> git clone https://github.com/curry-language/curry-info-system.git\n" ++
        "> cd curry-info-system\n" ++
        "> cypm install\n"
      exitWith 1

------------------------------------------------------------------------------
-- Generate analysis information for a given package and version.
-- In remote mode, the package repository index is also updated.
generateForPackage:: Options -> String -> String -> IO ()
generateForPackage opts pkg vsn = do
  let pkgid = pkg ++ "-" ++ vsn
  printWhenStatus opts $
    "Generating infos for package '" ++ pkgid ++ "' for " ++
    show (optEntity opts) ++ " entities..."
  when (optEntity opts == Unknown) $ exitWith 0
  let pkgvsnopts = [ ("--package", pkg), ("--version", vsn) ]
  when (null (optRequest opts)) $ do
    printWhenStatus opts $
      "Cleaning old information of package '" ++ pkgid ++ "'..."
    callCurryInfo opts (pkgvsnopts ++ [("--clean","")])
    when (optRemote opts) $ do  -- update repo index in remote mode:
      printWhenStatus opts "Update package repository index..."
      callCurryInfo opts [ ("--update","") ]
    callCurryInfo opts
      (("--package", pkg) : ("--force","2") : map (\r->(r,"")) packageRequests)
    callCurryInfo opts
      (pkgvsnopts ++ ("--force","2") : map (\r->(r,"")) packageVersionRequests)
  mods <- getPackageModules opts pkg vsn
  mapM_ (generateForModule opts pkg vsn) mods

-- Generate analysis information for a given package, version, and module.
generateForModule :: Options -> String -> String -> String -> IO ()
generateForModule opts pkg vsn mn = do
  genInfoMsg []
  let ciopts = [ ("--force","2"), ("--package", pkg), ("--version", vsn)
               , ("--module", mn) ]
  let optreqs = optRequest opts
  if null optreqs
    then do
      infoAndCallCurry ciopts moduleRequests
      mapM_ (genClassRequest ciopts) classRequests
      mapM_ (genTypeRequest  ciopts) typeRequests
      mapM_ (genOpRequest    ciopts) operationRequests
    else case optEntity opts of
      Class     -> mapM_ (genClassRequest ciopts) optreqs
      Type      -> mapM_ (genTypeRequest  ciopts) optreqs
      Operation -> mapM_ (genOpRequest    ciopts) optreqs
      Unknown   -> return ()
 where
  genClassRequest ciopts req
    -- for a `documentation` or `definition` request, compute it only for
    -- a dummy entity since this sets the results for all entities
    | req `elem` ["documentation", "definition"]
    = infoAndCallCurry ciopts ["--class=", req]
    | otherwise
    = infoAndCallCurry ciopts ["--allclasses", req]
    
  genTypeRequest ciopts req
    -- for a `documentation` or `definition` request, compute it only for
    -- a dummy entity since this sets the results for all entities
    | req `elem` ["documentation", "definition"]
    = infoAndCallCurry ciopts ["--type=", req]
    | otherwise
    = infoAndCallCurry ciopts ["--alltypes", req]
    
  genOpRequest ciopts req
    -- for operation requests (different from `name`), compute requests only
    -- for a dummy operation since this sets the analysis results
    -- for all operations
    | req `notElem` ["name"]
    = infoAndCallCurry ciopts ["--operation=", req]
    | otherwise
    = infoAndCallCurry ciopts ["--alloperations", req]
    
  infoAndCallCurry ciopts reqs = do
    genInfoMsg reqs
    callCurryInfo opts $ ciopts ++ map (\r->(r,"")) reqs

  genInfoMsg req = printWhenStatus opts $
    "Generating infos for '" ++ mn ++
    (if null req then "" else "' and '" ++ unwords req) ++ "'..."

------------------------------------------------------------------------------
-- Query an entity of a given module. The module is looked up in the
-- current load path of Curry if information about the package/version
-- is not provided.
queryModuleEntity :: Options -> String -> String -> IO ()
queryModuleEntity opts0 mname ename = do
  let opts1   = opts0 { optModule = mname, optEName = ename }
      entity  = optEntity opts1
      estring = if null ename
                  then bold "Module" ++ " " ++ code mname
                  else bold (show entity) ++ " " ++
                       code (mname ++ "." ++ ename)
  (pname,vers) <- if null (optPackage opts1) || null (optVersion opts1)
                    then getPackageVersion opts1 estring
                    else return (optPackage opts1, optVersion opts1)
  -- do something with package, version, module, and function:
  let edescr = estring ++ " (package " ++ pname ++ "-" ++ vers ++ ")"
  putStrLn (addBreak edescr)
  hFlush stdout -- required for KiCS2
  let requests = if null (optRequest opts1)
                   then if null ename && not (optAll opts1)
                          then [ "classes", "types", "operations" ]
                          else let rcreq = case entity of
                                             Class     -> optCRequests opts1
                                             Type      -> optTRequests opts1
                                             Operation -> optORequests opts1
                                             Unknown   -> []
                               in if null rcreq then defaultShowRequests entity
                                                else rcreq
                   else optRequest opts1
      opts2    = opts1 { optPackage = pname, optVersion = vers
                       , optRequest = requests }
  callCurryInfo opts2 (curryInfoOptions opts2)
 where
  -- Bold text when markdown option is set:
  bold s = if optMarkdown opts0 then "__" ++ s ++ "__" else s

  -- Code string when markdown option is set:
  code s = if optMarkdown opts0 then "`" ++ s ++ "`" else s

  -- Add a markdown line break, if required:
  addBreak s = s ++ if optMarkdown opts0 then "  " else ""

  getPackageVersion opts estring = do
    setCurryPathIfNecessary
    mbsrc <- lookupModuleSourceInLoadPath mname
    case mbsrc of
      Nothing -> error $ "Module '" ++ mname ++ "' not found!"
      Just (dirname,filename) -> do
        getPackageVersionOfDirectory dirname >>= maybe
          (putStrLn (addBreak estring) >> printWhenStatus opts
            ("Module '" ++ mname ++ "' " ++
            (if optVerb opts > 1
                then  "stored in file\n  " ++ filename ++
                      "\nbut this does not belong to the sources of a "
                else "does not belong to a ") ++
            "registered CPM package!") >> exitWith 0)
          return

-- Query a package specified in the option argument.
queryPackage :: Options -> IO ()
queryPackage opts0 = do
  let opts = opts0 { optModule = "", optEName = ""
                   , optAll = False, optShowAll = False
                   , optRequest = if null (optRequest opts0)
                                    then if null (optVersion opts0)
                                           then [ "versions" ]
                                           else [ "modules" ]
                                    else optRequest opts0 }
  callCurryInfo opts (curryInfoOptions opts)

-- Generate curry-info options from cpm-query options.
curryInfoOptions :: Options -> [(String,String)]
curryInfoOptions opts =
  [ ("--format", optOutFormat opts)
  , ("--force", if optShowAll opts then "0" else show (optForce opts)) ] ++
  (if optShowAll opts then [("--showall","")] else []) ++
  (if null (optPackage opts) then [] else [("--package", optPackage opts)]) ++
  (if null (optVersion opts) then [] else [("--version", optVersion opts)]) ++
  (if null (optModule  opts) then [] else [("--module",  optModule opts)]) ++
  (if optAll opts then allEntOpt
                  else if null ename then [] else entityOpt) ++
  map (\r -> (r,"")) (optRequest opts)
 where
  ename = optEName opts
  allEntOpt = case optEntity opts of
                Operation     -> [ ("--alloperations","") ]
                Type          -> [ ("--alltypes","")      ]
                Class         -> [ ("--allclasses","")    ]
                Unknown       -> []
  entityOpt = case optEntity opts of
                Operation     -> [ ("--operation", ename) ]
                Type          -> [ ("--type",      ename) ]
                Class         -> [ ("--class",     ename) ]
                Unknown       -> []

-- The command and parameters to invoke `curry-info` depending on the
-- options passed as the first parameter.
-- The verbosity and color options are set according to these options.
-- The second parameters are the actual options for the `curry-info` command.
curryInfoCmd :: Options -> [(String,String)] -> (String, [String])
curryInfoCmd opts ciopts =
  if optRemote opts
    then ("curl",
          ["--max-time", show (optMaxTime opts), "--silent", "--show-error",
           optRemoteURL opts ++ "?" ++ params2urlencoded (addOptions ciopts)])
    else (curryInfoBin,
          map (\(o,v) -> if null v then o else o ++ '=' : v)
              (addOptions (("--verbosity", show (optVerb opts)) : ciopts)))
 where
  addOptions = addCacheOption . addTextFormatOption

  -- Add the option `--color` or `--markdown` to a list of `curry-info` options,
  -- if demanded.
  addTextFormatOption ncopts | optMarkdown opts = ("--markdown","") : ncopts
                             | optColor opts    = ("--color","") : ncopts
                             | otherwise        = ncopts

  -- Add the option `--cache` to a list of `curry-info` options
  -- if the `curryInfoCache` is not null.
  addCacheOption ncopts
    | null curryInfoCache = ncopts
    | otherwise           = ("--cache", curryInfoCache) : ncopts

-- Calls `curry-info` locally or in remote mode with the given parameters.
-- In dry mode, show only the command.
callCurryInfo :: Options -> [(String,String)] -> IO ()
callCurryInfo opts ciopts = do
  let (cmdbin,cmdopts) = curryInfoCmd opts ciopts
      cmd = unwords $ cmdbin : map escapeShellString cmdopts
  when (optVerb opts > 1 || optDryRun opts) $ putStrLn $ "Executing: " ++ cmd
  unless (optDryRun opts) $ do
    ec <- system cmd
    when (ec > 0) $
      if optRemote opts && ec == 28 -- timeout return code of `curl`
        then putStrLn "(timeout from CurryInfo web service)"
        else printWhenStatus opts $ "Execution error! Return code: " ++ show ec

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
---
--- Example: get the demanded arguments of all operation in module `Data.List`:
---
---     > askCurryInfoServer "Data.List" Operation "demand"
---
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
--- of the given request (fifth argument) for all entities in the module
--- provided as the third argument. The requested result is returned in its
--- string representation.
--- If the first argument is `True`, the `curry-info` web service is queried.
--- The second is the verbosity (where 0 means quiet).
--- The third argument is the kind of entity to be queried.
--- If it is `Unknown`, `Nothing` is returned.
--- The sixth argument specifies the output format, e.g.,
--- "Text", "JSON", "CurryTerm", or "CurryMap".
--- 
--- The package and version are determined using the Curry loadpath.
--- If something goes wrong, Nothing is returned.
---
--- Example: get the demanded arguments of all operations in module `Data.List`:
---
---     > askCurryInfoCmd True 1 "Data.Maybe" Operation "demand" "JSON"
---
askCurryInfoCmd :: Bool -> Int -> String -> CurryEntity -> String -> String
                -> IO (Maybe String)
askCurryInfoCmd remote verb modname entkind req outformat
  | entkind == Unknown = return Nothing
  | otherwise = do
    mres <- getPackageVersionOfModule modname
    case mres of
      Nothing -> return Nothing
      Just (pkg, vsn) -> do
        -- Note: force=0 is important to avoid loops if the analysis tools
        -- also use `curry-info`!
        -- Verbosity 0/quiet is also important, otherwise buffer for stderr
        -- will be filled in the `evalCmd` call so that it might overflow.
        let queryopts = defaultOptions
                          { optForce = 0, optVerb = 0, optAll = True
                          , optRemote = remote, optRemoteURL = curryInfoURL
                          , optOutFormat = outformat
                          , optPackage = pkg, optVersion = vsn
                          , optModule = modname
                          , optRequest = [req] }
            (cmd,cmdopts) = curryInfoCmd queryopts (curryInfoOptions queryopts)
        when (verb > 1) $ hPutStrLn stderr $
          unwords $ ["Executing:", cmd] ++ cmdopts
        (ec, out, err) <- evalCmd cmd cmdopts ""
        if ec > 0
          then do when (verb > 0) $ do
                    hPutStrLn stderr $ line ++ "Execution error. Output:"
                    unless (null out) $ hPutStrLn stderr out
                    unless (null err) $ hPutStrLn stderr err
                    hPutStrLn stderr line
                  return Nothing
          else return $ Just out
 where
  line = take 70 (repeat '-') ++ "\n"

----------------------------------------------------------------------------
--- Query `curry-info` with some request where the options are taken
--- from the RC file.
--- The first argument are the `curry-info` arguments to specify the entity
--- (e.g., package/version/module/operation).
--- The second argument is the (single!) request.
--- The result must be readable so that it it is returned as data.
--- In case of a parse error, the program is terminated with an error code.
queryCurryInfo :: Read a => [(String,String)] -> String -> IO a
queryCurryInfo especs req = do
  opts <- getDefaultOptions
  queryCurryInfoWithOptions opts especs req

--- Query `curry-info` with some request.
--- The first argument are the `cpm-query` options to be used for the query.
--- The second argument are the `curry-info` arguments to specify the entity
--- (e.g., package/version/module/option).
--- The third argument is the (single!) request.
--- The result must be readable so that it it is returned as data.
queryCurryInfoWithOptions :: Read a => Options -> [(String,String)] -> String
                          -> IO a
queryCurryInfoWithOptions opts especs req = do
  let ciopts = [("--verbosity",show (optVerb opts)), ("--format","CurryTerm")]
               ++ especs ++ [(req,"")]
      (cmd,cmdopts) = curryInfoCmd opts ciopts
  printWhenAll opts $ unwords $ ["Executing:", cmd] ++ cmdopts
  (ec,sout,serr) <- evalCmd cmd cmdopts ""
  when (optVerb opts > 2 || ec > 0) $ putStrLn $
    "Exit code: " ++ show ec ++ "\nSTDOUT:\n" ++ sout ++ "\nSTDERR:\n" ++ serr
  when (ec > 0) $ reportError "ERROR OCCURRED"
  -- Now we parse the output of curry-info:
  -- curry-info returns a list of strings pairs (package string, info string):
  resterm <- case reads sout of
               [(ms,_)] -> return (ms :: [(String,[(String,String)])])
               _ -> reportError $ "Parse error for: " ++ sout
  -- the info string is a pair of requests and results as strings:
  case resterm of
    [] -> reportError "NO INFORMATION FOUND!"
    ((_,(_,infostring):_):_) -> do
      -- since we had only a single request, we read (first) result string:
      case reads infostring of
        [(ms,_)] -> return ms
        _        -> reportError ("Parse error for: " ++ infostring)
    _  -> reportError $ "UNEXPECTED RESULT: " ++ sout
 where
  reportError s = putStrLn s >> exitWith 1

-- Examples to use `queryCurryInfoWithOptions`:

--- Get all modules of a package version by `curry-info`.
--- In case of a parse error, the program is terminated with an error state.
getPackageModules :: Options -> String -> String -> IO [String]
getPackageModules opts pkg vsn = do
  printWhenStatus opts $
    "Get modules of package '" ++ pkg ++ "-" ++ vsn ++ "'..."
  queryCurryInfoWithOptions opts --{ optVerb = 0 }
    [("--force","0"), ("--package",pkg), ("--version",vsn)] "modules"

--- Get the number of operations of all modules of a package version.
--- In case of a parse error, the program is terminated with an error state.
getPackageModuleOps :: Options -> String -> String -> IO [(String,Int)]
getPackageModuleOps opts pkg vsn = do
  let qopts = opts { optVerb = 0 }
  mods <- getPackageModules opts pkg vsn
  mops <- mapM (\m -> (queryCurryInfoWithOptions qopts
                         [ ("--force","0"), ("--package",pkg), ("--version",vsn)
                         , ("--module",m)]
                         "operations"))
               mods
  return (zip mods (map length (mops :: [[String]])))
    -- >>= return . foldr (+) 0 . map snd -- to add all numbers

----------------------------------------------------------------------------
-- Auxiliaries:

-- Is the string a valid package name?
isPackageName :: String -> Bool
isPackageName []     = False
isPackageName (c:cs) = isAlphaNum c &&
                       all (\d -> isAlphaNum d || d == '-' || d == '_') cs

--- The binary name of the curry-info tool.
curryInfoBin :: String
curryInfoBin = "curry-info"

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

-- Escape a string to use it as a parameter in a shell command.
escapeShellString :: String -> String
escapeShellString s
  | all isAlphaNum s = s
  | null s           = "''"
  | otherwise        = '\'' : concatMap escapeSingleQuote s ++ "'"
 where
  escapeSingleQuote c | c == '\'' = "'\\\''"
                      | otherwise = [c]

----------------------------------------------------------------------------
