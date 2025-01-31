-------------------------------------------------------------------------
--- Configuration parameters of the CPM querying tool.
---
--- @author Michael Hanus
--- @version January 2025
-------------------------------------------------------------------------

module CPM.Query.Configuration
  ( CurryEntity(..), defaultShowRequests, curryInfoURL
  , packageVersionRequests, moduleRequests
  , classRequests, typeRequests, operationRequests
  )
 where

--- The kind of entity in a Curry module to be queried.
data CurryEntity = Operation | Type | Class | Unknown
  deriving (Eq, Show)

--- The default requests to be shown for various kinds entities.
defaultShowRequests :: CurryEntity -> [String]
defaultShowRequests cent = case cent of
  Operation  -> [ "documentation", "cass-deterministic", "cass-total"
                , "cass-terminating", "cass-demand", "failfree" ]
  Type       -> [ "documentation", "definition" ]
  Class      -> [ "documentation", "definition" ]
  Unknown    -> []

--- The URL of the `curry-info` web service CGI script.
curryInfoURL :: String
curryInfoURL = "https://cpm.curry-lang.org/webapps/curry-info/run.cgi"

--- The requests to be generated for package versions.
packageVersionRequests :: [String]
packageVersionRequests =
  ["documentation", "categories", "dependencies", "modules", "exportedmodules"]

--- The requests to be generated for modules.
moduleRequests :: [String]
moduleRequests =
  [ "documentation", "sourcecode", "cass-unsafemodule"
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
  [ "documentation", "definition", "signature", "infix", "precedence"
  , "cass-demand", "cass-deterministic", "cass-indeterministic"
  , "cass-solcomplete", "cass-terminating", "cass-total", "cass-values"
  , "failfree", "iotype" ]
