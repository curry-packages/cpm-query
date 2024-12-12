-------------------------------------------------------------------------
--- The options of the CPM querying tool.
---
--- @author Michael Hanus
--- @version November 2024
-------------------------------------------------------------------------

module CPM.Query.Options
  ( CurryEntity(..), Options(..), defaultOptions, processOptions
  , usageText
  , printWhenStatus, printWhenIntermediate, printWhenAll
  )
 where

import Control.Monad         ( when, unless )
import Data.Char             ( toLower )
import Numeric               ( readNat )
import System.Console.GetOpt

import System.Process        ( exitWith )

--- The kind of entity of a Curry module to be queried.
data CurryEntity = Operation | Type | TypeClass | Unknown
  deriving (Eq, Show)

-- The options of the query tool.
data Options = Options
  { optVerb      :: Int         -- verbosity (0: quiet, 1: status,
                                --            2: intermediate, 3: all)
  , optHelp      :: Bool        -- if help info should be printed
  , optEntity    :: CurryEntity -- show the result for this kind of entity
  , optCLS       :: String      -- entity kind passed by Curry language server
  , optDryRun    :: Bool        -- dry run, i.e., do not invoke curry-info?
  , optForce     :: Bool        -- force computation of analysis information?
  , optGenerate  :: Bool        -- generate information for a package version?
  , optRequest   :: [String]    -- specific requests for the entity?
  , optOutFormat :: String      -- output format
  }

--- The default options of the query tool.
defaultOptions :: Options
defaultOptions = Options 1 False Operation "" False False False [] "Text"

--- Process the actual command line arguments and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (Options,[String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  return (opts, args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: cpm-query [options] <module names> <entity name>\n" ++
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
           "run quietly (no status output, only exit code)"
  , Option "v" ["verbosity"]
           (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
           "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show more details (same as `-v')\n3: show all details"
  , Option "f" ["function"]
           (NoArg (\opts -> opts { optEntity = Operation }))
          "show information about an operation (default)"
  , Option "t" ["type"]
           (NoArg (\opts -> opts { optEntity = Type }))
          "show information about a type"
  , Option "c" ["typeclass"]
           (NoArg (\opts -> opts { optEntity = TypeClass }))
           "show information about a type class"
  , Option "" ["clskind"]
           (ReqArg checkKind "<k>")
           "entity kind provided by the Curry language server\n(ValueFunction|TypeData|TypeClass|...)"
  , Option "" ["force"]
           (NoArg (\opts -> opts { optForce = True }))
           "force computation of properties"
  , Option "d" ["dry"]
           (NoArg (\opts -> opts { optDryRun = True }))
           "dry run, i.e., do not run `curry-info` analyses"
  , Option "" ["generate"]
           (NoArg (\opts -> opts { optGenerate = True }))
           "generate analysis infos for a package version"
  , Option "" ["request"]
           (ReqArg (\r opts -> opts { optRequest = optRequest opts ++ [r] })
                   "<r>")
           "specific request (e.g., definition)\n(multiple options allowed)"
  , Option "" ["output"]
           (ReqArg checkFormat "<f>")
           "output format: Text (default), JSON, CurryTerm"
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
    | k == "TypeClass"     = opts' { optEntity = TypeClass }
    | otherwise            = opts' { optEntity = Unknown }
   where opts' = opts { optCLS = k }

  checkFormat f opts =
    if map toLower f `elem` ["text", "json", "curryterm"]
      then opts { optOutFormat = f }
      else error "Illegal output format (try `-h' for help)"

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
