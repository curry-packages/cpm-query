-------------------------------------------------------------------------
--- Configuration parameters of the CPM querying tool.
---
--- @author Michael Hanus
--- @version February 2025
-------------------------------------------------------------------------

module CPM.Query.Configuration
  ( CurryEntity(..), defaultShowRequests, curryInfoURL, curryInfoCache
  , packageVersionRequests, moduleRequests
  , classRequests, typeRequests, operationRequests, moduleOperationsRequests
  )
 where

--- The kind of entity in a Curry module to be queried.
data CurryEntity = Operation | Type | Class | Unknown
  deriving (Eq, Show)

--- The default requests to be shown for various kinds entities.
defaultShowRequests :: CurryEntity -> [String]
defaultShowRequests cent = case cent of
  Operation  -> [ "documentation", "deterministic", "totally-defined"
                , "terminating", "demand", "failfree" ]
  Type       -> [ "documentation", "definition" ]
  Class      -> [ "documentation", "definition" ]
  Unknown    -> []

--- The URL of the `curry-info` web service CGI script.
curryInfoURL :: String
curryInfoURL = "https://cpm.curry-lang.org/webapps/curry-info/run.cgi"

--- The cache directory passed to `curry-info` (if not null).
curryInfoCache :: String
curryInfoCache = ""
--curryInfoCache = "$HOME/CURRYINFOCACHE"

--- The requests to be generated for package versions.
packageVersionRequests :: [String]
packageVersionRequests =
  ["documentation", "categories", "dependencies", "modules", "exportedmodules"]

--- The requests to be generated for modules.
moduleRequests :: [String]
moduleRequests =
  [ "documentation", "sourcecode", "unsafe"
  , "classes", "types", "operations" ]

--- The requests to be generated for classes.
classRequests :: [String]
classRequests = ["documentation", "methods", "definition"]

--- The requests to be generated for types.
typeRequests :: [String]
typeRequests = ["documentation", "constructors", "definition"]

--- The requests to be generated for operations.
operationRequests :: [String]
operationRequests =
  [ "documentation", "definition", "signature", "infix", "precedence" ] ++
  moduleOperationsRequests

--- Requests which analyze all operations of a module at once.
moduleOperationsRequests :: [String]
moduleOperationsRequests =
  [ "demand", "deterministic", "indeterministic"
  , "solution-complete", "terminating", "totally-defined", "result-values"
  , "failfree", "iotype" ]
