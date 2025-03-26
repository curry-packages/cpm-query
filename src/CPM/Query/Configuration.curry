-------------------------------------------------------------------------
--- Configuration parameters of the CPM querying tool.
---
--- @author Michael Hanus
--- @version March 2025
-------------------------------------------------------------------------

module CPM.Query.Configuration
  ( CurryEntity(..), defaultShowRequests, curryInfoURL, curryInfoCache
  , packageRequests, packageVersionRequests, moduleRequests
  , classRequests, typeRequests, operationRequests
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

--- The requests to be generated for packages.
packageRequests :: [String]
packageRequests = [ "name", "versions" ]

--- The requests to be generated for package versions.
packageVersionRequests :: [String]
packageVersionRequests =
  [ "version", "documentation", "categories", "dependencies"
  , "modules", "exportedmodules" ]

--- The requests to be generated for modules.
moduleRequests :: [String]
moduleRequests =
  [ "name", "documentation", "sourcecode", "unsafe"
  , "classes", "types", "operations" ]

--- The requests to be generated for classes.
classRequests :: [String]
classRequests = ["name", "documentation", "definition", "methods"]

--- The requests to be generated for types.
typeRequests :: [String]
typeRequests = ["name", "documentation", "definition", "constructors"]

--- The requests to be generated for operations.
operationRequests :: [String]
operationRequests =
  [ "name", "documentation", "definition", "signature", "infix", "precedence"
  , "demand", "deterministic", "indeterministic"
  , "solution-complete", "terminating", "totally-defined", "result-values"
  , "failfree", "iotype" ]
