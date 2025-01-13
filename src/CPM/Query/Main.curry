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
  --( main, askCurryInfoServer, askCurryInfoCmd, getPackageModules )
 where

import Control.Monad      ( unless, when, replicateM )
import Data.List          ( intercalate, isPrefixOf, nub, union )
import System.Environment ( getArgs )
import System.IO          ( getContents, hFlush, stderr, hClose, hGetLine
                          , hPutStrLn )

import FlatCurry.Types    ( QName )
import Network.Socket     ( connectToSocket )
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

import CPM.Query.Configuration
import CPM.Query.Options

---------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "CPM Query Tool (Version of 13/01/25)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  (opts,args) <- getArgs >>= processOptions banner
  unless (optRemote opts) checkExecutable
  when (optEntity opts == Unknown) $ do
    printWhenStatus opts $
      "No information for entity of kind '" ++ optCLS opts ++ "'"
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
            -> do ls <- if genfile == "-" then getContents else readFile genfile
                  mapM_ (genFromLine opts) -- ignore comments starting with #
                        (nub (filter (\l -> take 1 l /= "#") (lines ls)))
    _       -> do putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n\n" ++
                             usageText
                  exitWith 1
 where
  genFromLine opts l = case words l of
    [p,v] -> generateForPackage opts p v
    []    -> return () -- skip empty lines 
    _     -> hPutStrLn stderr $ "Ignore illegal line in generate file: " ++ l

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
  let pkgvsnopts = [ "--package=" ++ pkg, "--version=" ++ vsn ]
  when (null (optRequest opts)) $ do
    printWhenStatus opts $
      "Cleaning old information of package '" ++ pkgid ++ "'..."
    callCurryInfo opts (pkgvsnopts ++ ["--clean"])
    when (optRemote opts) $ do  -- update repo index in remote mode:
      printWhenStatus opts "Update package repository index..."
      callCurryInfo opts [ "--update" ]
    callCurryInfo opts [ "--force=2", "--package=" ++ pkg, "versions" ]
    callCurryInfo opts (pkgvsnopts ++ "--force=2" : packageVersionRequests)
  mods <- getPackageModules opts pkg vsn
  mapM_ (generateForModule opts pkg vsn) mods

-- Generate analysis information for a given package, version, and module.
generateForModule :: Options -> String -> String -> String -> IO ()
generateForModule opts pkg vsn mn = do
  genInfoMsg []
  let ciopts = [ "--force=2", "--package=" ++ pkg, "--version=" ++ vsn
               , "--module="  ++ mn ]
  let optreqs = optRequest opts
  if null optreqs
    then do
      infoAndCallCurry ciopts ["documentation", "sourcecode"]
      infoAndCallCurry ciopts ("--allclasses" : classRequests)
      infoAndCallCurry ciopts ("--alltypes"  : typeRequests)
      mapM_ (genOpRequest ciopts) operationRequests
    else case optEntity opts of
      Class     -> infoAndCallCurry ciopts ("--allclasses" : optreqs)
      Type      -> infoAndCallCurry ciopts ("--alltypes"   : optreqs)
      Operation -> mapM_ (genOpRequest ciopts) optreqs
      Unknown   -> return ()
 where
  -- is the request one which generates infos for all operations at once?
  singleOpRequest req = "cass-" `isPrefixOf` req || req == "failfree"

  genOpRequest ciopts req
    -- for a singleOpRequest, compute request only for dummy operation since
    -- this sets the analysis results for all operations:
    | singleOpRequest req = infoAndCallCurry ciopts ["--operation=", req]
    | otherwise           = infoAndCallCurry ciopts ["--alloperations", req]
    
  infoAndCallCurry ciopts reqs = do
    genInfoMsg reqs
    callCurryInfo opts $ ciopts ++ reqs

  genInfoMsg req = printWhenStatus opts $
    "Generating infos for '" ++ mn ++
    (if null req then "" else "' and '" ++ unwords req) ++ "'..."

------------------------------------------------------------------------------
-- Query an entity of a given module. The module is looked up in the
-- current load path of Curry if information about the package/version
-- is not provided.
queryModuleEntity :: Options -> String -> String -> IO ()
queryModuleEntity opts0 mname ename = do
  let opts1 = opts0 { optModule = mname, optEName = ename }
  (pname,vers) <- if null (optPackage opts1) || null (optVersion opts1)
                    then getPackageVersion opts1
                    else return (optPackage opts1, optVersion opts1)
  -- do something with package, version, module, and function:
  let entity = optEntity opts1
      edescr = if optVerb opts1 > 1
                 then unlines [ "Package name   : " ++ pname
                              , "Package version: " ++ vers
                              , "Module name    : " ++ mname
                              , "Entity name    : " ++ ename ]
                 else (if null ename
                         then "Module " ++ mname
                         else show entity ++ " " ++ mname ++ "." ++ ename) ++
                      " (package " ++ pname ++ "-" ++ vers ++ ")"
  putStrLn edescr
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
  getPackageVersion opts = do
    setCurryPathIfNecessary
    mbsrc <- lookupModuleSourceInLoadPath mname
    case mbsrc of
      Nothing -> error $ "Module '" ++ mname ++ "' not found!"
      Just (dirname,filename) -> do
        getPackageVersionOfDirectory dirname >>= maybe
          (printWhenStatus opts
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
curryInfoOptions :: Options -> [String]
curryInfoOptions opts =
  [ "--format=" ++ optOutFormat opts
  , if optShowAll opts then "-f0" else "-f" ++ show (optForce opts) ] ++
  (if optShowAll opts then ["--showall"] else []) ++
  (if null (optPackage opts) then [] else ["--package=" ++ optPackage opts]) ++
  (if null (optVersion opts) then [] else ["--version=" ++ optVersion opts]) ++
  (if null (optModule  opts) then [] else ["--module="  ++ optModule opts]) ++
  (if optAll opts then allEntOpt
                  else if null ename then [] else entityOpt) ++
  optRequest opts
 where
  ename = optEName opts
  allEntOpt = case optEntity opts of
                Operation     -> [ "--alloperations" ]
                Type          -> [ "--alltypes"      ]
                Class         -> [ "--allclasses"    ]
                Unknown       -> []
  entityOpt = case optEntity opts of
                Operation     -> [ "--operation=" ++ ename ]
                Type          -> [ "--type="      ++ ename ]
                Class         -> [ "--class="     ++ ename ]
                Unknown       -> []

-- The command and parameters to invoke `curry-info` depending on the
-- options passed as the first parameter.
-- The verbosity and color options are set according to these options.
-- The second parameters are the actual options for the `curry-info` command.
curryInfoCmd :: Options -> [String] -> (String, [String])
curryInfoCmd opts ciopts =
  if optRemote opts
    then ("curl",
          ["--max-time", "3600", "--silent", "--show-error",
           optRemoteURL opts ++ "?" ++
           intercalate "&" (addColorOption (map string2urlencoded ciopts))])
    else (curryInfoBin,
          addColorOption ["--verbosity=" ++ show (optVerb opts)] ++ ciopts)
 where
  -- Add the option `--color` to a list of `curry-info` options, if demanded.
  addColorOption ncopts = if optColor opts then "--color" : ncopts else ncopts

-- Calls `curry-info` locally or in remote mode with the given parameters.
-- In dry mode, show only the command.
callCurryInfo :: Options -> [String] -> IO ()
callCurryInfo opts ciopts = do
  let (cmdbin,cmdopts) = curryInfoCmd opts ciopts
      cmd = unwords $ cmdbin : map escapeShellString cmdopts
  when (optVerb opts > 1 || optDryRun opts) $ putStrLn $ "Executing: " ++ cmd
  unless (optDryRun opts) $ do
    ec <- system cmd
    when (ec > 0) $
      printWhenStatus opts $ "Execution error! Return code: " ++ show ec

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
--- If the first argument is `True`, the `curry-info` server is queried.
--- The third argument is the kind of entity to be queried.
--- If it is `Unknown`, `Nothing` is returned.
--- 
--- The package and version are determined using the Curry loadpath.
--- If something goes wrong, Nothing is returned.
askCurryInfoCmd :: Bool -> String -> CurryEntity -> String
                -> IO (Maybe [(QName, String)])
askCurryInfoCmd useserver modname entkind req
  | entkind == Unknown = return Nothing
  | otherwise = do
    mres <- getPackageVersionOfModule modname
    case mres of
      Nothing -> return Nothing
      Just (pkg, vsn) -> do
        -- Note: force=0 is important to avoid loops if the analysis tools
        -- also use `curry-info`!
        let queryopts = defaultOptions
                          { optForce = 0, optVerb = 0, optAll = True
                          , optRemote = useserver, optRemoteURL = curryInfoURL
                          , optOutFormat = "CurryTerm"
                          , optPackage = pkg, optVersion = vsn
                          , optModule = modname
                          , optRequest = [req] }
            (cmd,cmdopts) = curryInfoCmd queryopts (curryInfoOptions queryopts)
        (ec, out, err) <- evalCmd cmd cmdopts ""
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
--- Query `curry-info` with some request where the options are taken
--- from the RC file.
--- The first argument are the `curry-info` arguments to specify the entity
--- (e.g., package/version/module/option).
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
  let ciopts = [ "-v" ++ show (optVerb opts), "--format=CurryTerm" ] ++
               map (\(t,v) -> t ++ "=" ++ v) especs ++ [req]
      (cmd,cmdopts) = curryInfoCmd opts ciopts
  printWhenAll opts $ unwords $ ["Executing:", cmd] ++ cmdopts
  (ec,sout,serr) <- evalCmd cmd cmdopts ""
  when (optVerb opts > 2 || ec > 0) $ putStrLn $
    "Exit code: " ++ show ec ++ "\nSTDOUT:\n" ++ sout ++ "\nSTDERR:\n" ++ serr
  when (ec > 0) $ reportError "ERROR OCCURRED"
  -- Now we parse the output of curry-info:
  -- curry-info returns a list of strings pairs (package string, info string):
  resterm <- case reads sout of
               [(ms,_)] -> return (ms :: [(String,String)])
               _ -> reportError $ "Parse error for: " ++ sout
  -- the info string is a pair of requests and results as strings:
  case resterm of
    [] -> reportError "NO INFORMATION FOUND!"
    ((_,infos):_) -> do
      infolist <- case reads infos of
                    [(ms,_)] -> return (ms :: [(String,String)])
                    _ -> reportError ("Parse error for: " ++ infos)
      -- finally, the (first) result string is a list of module names:
      let infostring = snd (head infolist)
      case reads infostring of
        [(ms,_)] -> return ms
        _        -> reportError ("Parse error for: " ++ infostring)
 where
  reportError s = putStrLn s >> exitWith 1

-- Examples to use `queryCurryInfoWithOptions`:

--- Get all modules of a package version by `curry-info`.
--- In case of a parse error, the program is terminated with an error state.
getPackageModules :: Options -> String -> String -> IO [String]
getPackageModules opts pkg vsn = do
  printWhenStatus opts $
    "Get modules of package '" ++ pkg ++ "-" ++ vsn ++ "'..."
  queryCurryInfoWithOptions opts { optVerb = 0 }
    [("--package",pkg), ("--version",vsn)] "modules"

--- Get the number of operations of all modules of a package version.
--- In case of a parse error, the program is terminated with an error state.
getPackageModuleOps :: Options -> String -> String -> IO [(String,Int)]
getPackageModuleOps opts pkg vsn = do
  let qopts = opts { optVerb = 0 }
  mods <- getPackageModules opts pkg vsn
  mops <- mapM (\m -> (queryCurryInfoWithOptions qopts
                         [("--package",pkg), ("--version",vsn), ("--module",m)]
                         "operations"))
               mods
  return (zip mods (map length (mops :: [[String]])))
    -- >>= return . foldr (+) 0 . map snd -- to add all numbers

----------------------------------------------------------------------------
-- Auxiliaries:

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

-- Translates arbitrary strings into equivalent URL encoded strings.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | noEncode c = c : string2urlencoded cs
  | c == ' '   = '+' : string2urlencoded cs
  | otherwise
  = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
  noEncode x = isAlphaNum x || x `elem` "=-"

  int2hex i = if i<10 then chr (ord '0' + i)
                      else chr (ord 'A' + i - 10)

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
