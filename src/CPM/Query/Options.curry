-------------------------------------------------------------------------
--- The options of the CPM querying tool.
---
--- @author Michael Hanus
--- @version January 2025
-------------------------------------------------------------------------

module CPM.Query.Options
  ( CurryEntity(..), Options(..), defaultOptions, getDefaultOptions
  , processOptions, usageText
  , printWhenStatus, printWhenIntermediate, printWhenAll
  )
 where

import Control.Monad         ( when, unless )
import Data.Char             ( toLower )
import Data.List             ( findIndices, isPrefixOf, splitOn )
import Numeric               ( readNat )
import System.Console.GetOpt
import System.IO             ( hIsTerminalDevice, stdout )

import System.Process        ( exitWith )

import CPM.Query.Configuration
import CPM.Query.RCFile      ( readRC, rcValue )

-- The options of the query tool.
data Options = Options
  { optVerb      :: Int         -- verbosity (0: quiet, 1: status,
                                --            2: intermediate, 3: all)
  , optHelp      :: Bool        -- if help info should be printed
  , optPackage   :: String      -- the requested package
  , optVersion   :: String      -- the requested version
  , optModule    :: String      -- the requested module
  , optEName     :: String      -- the name of the requested entity
  , optEntity    :: CurryEntity -- show the result for this kind of entity
  , optCLS       :: String      -- entity kind passed by Curry language server
  , optAll       :: Bool        -- show information for all entities in a module
  , optColor     :: Bool        -- use colors in text output?
  , optDryRun    :: Bool        -- dry run, i.e., do not invoke curry-info?
  , optForce     :: Int         -- force computation of analysis information?
                                -- (0: no gen., 1: only if missing, 2: always)
  , optGenerate  :: Bool        -- generate information for a package version?
  , optGenFrom   :: String      -- file containing package/versions to generate
  , optCRequests :: [String]    -- default class requests
  , optTRequests :: [String]    -- default type requests
  , optORequests :: [String]    -- default operation requests
  , optRequest   :: [String]    -- specific requests for the entity?
  , optOutFormat :: String      -- output format
  , optShowAll   :: Bool        -- show all available information
  , optRemote    :: Bool        -- use curry-info web service for requests?
  , optRemoteURL :: String      -- URL of the curry-info web service
  , optMaxTime   :: Int         -- max-time (in seconds) for `curl` connections
  }

--- Default value for option `--maxtime` (will be increased if `--generate`
--- is set).
defaultMaxTime :: Int
defaultMaxTime = 30

--- The default options of the query tool.
defaultOptions :: Options
defaultOptions =
  Options 1 False "" "" "" "" Operation "" False False False 0 False ""
          [] [] [] [] "Text" False True "" defaultMaxTime

--- The default options with values from the RC file taken into account.
getDefaultOptions :: IO Options
getDefaultOptions = do
  rcprops <- readRC
  return $
    defaultOptions
      { optCRequests = readReqs (rcValue rcprops "classrequests")
      , optTRequests = readReqs (rcValue rcprops "typerequests")
      , optORequests = readReqs (rcValue rcprops "operationrequests")
      , optShowAll   = if rcValue rcprops "showall"== "yes" then True else False
      , optRemote    = if rcValue rcprops "remote" == "yes" then True else False
      , optRemoteURL = let rcurl = rcValue rcprops "curryinfourl"
                       in if null rcurl then curryInfoURL else rcurl
      }
 where
  readReqs s = if null s then [] else splitOn "," s

--- Process the actual command line arguments and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (Options,[String])
processOptions banner argv = do
  dfltoptions <- getDefaultOptions
  isterminal <- hIsTerminalDevice stdout
  let opts0 = dfltoptions { optColor = isterminal }
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts1 = foldl (flip id) opts0 funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts1) (printUsage >> exitWith 0)
  when (not (null (optGenFrom opts1)) && not (optGenerate opts1))
       (putStrLn "Superfluous file with generate data!" >> exitWith 1)
  let opts2 = -- Generate on the web server only if --remote is explicitly used:
              if optGenerate opts1 && "--remote" `notElem` argv
                then opts1 { optRemote = False }
                else opts1
  return (opts2, args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: cpm-query [options] <module name> <entity name>\n" ++
             "       cpm-query [options] <module name>\n" ++
             "       cpm-query [options]\n" ++
             "       cpm-query [options] --generate <package> <version>\n" ++
             "       cpm-query [options] --generate <package> <version> <mod>\n")
            options

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output beside info result)"
  , Option "v" ["verbosity"]
           (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
           "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show more details (same as `-v')\n3: show all details"
  , Option "p" ["package"]
       (ReqArg (\arg opts -> opts { optPackage = arg }) "<pkg>")
       "requested package"
  , Option "x" ["version"]
       (ReqArg (\arg opts -> opts { optVersion = arg }) "<vsn>")
       "requested version"
  , Option "m" ["module"]
       (ReqArg (\arg opts -> opts { optModule = arg }) "<mod>")
       "requested module"
  , Option "t" ["type"]
           (NoArg (\opts -> opts { optEntity = Type }))
          "show information about a type"
  , Option "c" ["class"]
           (NoArg (\opts -> opts { optEntity = Class }))
           "show information about a type class"
  , Option "o" ["operation"]
           (NoArg (\opts -> opts { optEntity = Operation }))
          "show information about an operation (default)"
  , Option "" ["clskind"]
           (ReqArg checkKind "<k>")
           "entity kind provided by the Curry language server\n(ValueFunction|TypeData|Class|...)"
  , Option "" ["all"]
           (NoArg (\opts -> opts { optAll = True }))
           "show information of all entities in a module"
  , Option "" ["color"]
           (NoArg (\opts -> opts { optColor = True }))
           "use colors in text output (default if terminal)"
  , Option "" ["nocolor"]
           (NoArg (\opts -> opts { optColor = False }))
           "do not use colors in text output"
  , Option "d" ["dry"]
           (NoArg (\opts -> opts { optDryRun = True }))
           "dry run, i.e., do not run `curry-info` analyses"
  , Option "" ["force"]
           (NoArg (\opts -> opts { optForce = 1 }))
           "force generation of properties"
  , Option "" ["generate"]
           (NoArg (\opts -> opts { optGenerate = True, optMaxTime = 3600 }))
           "generate analysis infos for a package version"
  , Option "" ["from"]
           (ReqArg (\f opts -> opts { optGenFrom = f }) "<f>")
           "file with generate data (in CSV format)"
  , Option "" ["request"]
           (ReqArg (\r opts -> opts { optRequest = optRequest opts ++
                                                   splitOn "," r })
                   "<r>")
           "specific request (e.g., definition)\n(separate multiple requests by comma)"
  , Option "" ["format"]
           (ReqArg checkFormat "<f>")
           "output format: Text (default), JSON, CurryTerm"
  , Option "" ["showall"]
           (NoArg (\opts -> opts { optShowAll = True }))
           "show all available information (no generation)"
  , Option "" ["local"]
           (NoArg (\opts -> opts { optRemote = False }))
           "use local installation of 'curry-info'"
  , Option "" ["remote"]
           (NoArg (\opts -> opts { optRemote = True }))
           "use 'curry-info' web service to fetch information\n(default)"
  , Option "" ["maxtime"]
           (ReqArg (safeReadNat (\n opts -> opts { optMaxTime = n })) "<t>")
           ("maximum time (in seconds) for remote web service\n(default: " ++
            show defaultMaxTime ++ ")")
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
    [(n,"")] -> opttrans n opts
    _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n >= 0 && n <= 3
                       then opts { optVerb = n }
                       else error "Illegal verbosity level (try `-h' for help)"

  checkKind k opts
    | k == "ValueFunction" = opts' { optEntity = Operation }
    | k == "TypeData"      = opts' { optEntity = Type }
    | k == "TypeAlias"     = opts' { optEntity = Type }
    | k == "TypeClass"     = opts' { optEntity = Class }
    | otherwise            = opts' { optEntity = Unknown }
   where opts' = opts { optCLS = k }

  checkFormat f opts =
    case findIndices (map toLower f `isPrefixOf`)
                     (map (map toLower) outputFormats) of
      []  -> error "Illegal output format (try `-h' for help)"
      [i] -> opts { optOutFormat = outputFormats !! i }
      _   -> error "Output format ambiguous (try `-h' for help)"

outputFormats :: [String]
outputFormats = ["Text", "JSON", "CurryTerm"]

-------------------------------------------------------------------------

printWhenStatus :: Options -> String -> IO ()
printWhenStatus opts s = when (optVerb opts > 0) (putStrLn s)

printWhenIntermediate :: Options -> String -> IO ()
printWhenIntermediate opts s =
  when (optVerb opts > 1) (putStrLn s)

printWhenAll :: Options -> String -> IO ()
printWhenAll opts s =
 when (optVerb opts > 2) (putStrLn s)

---------------------------------------------------------------------------
