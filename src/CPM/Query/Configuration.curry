-------------------------------------------------------------------------
--- Configuration parameters of the CPM querying tool.
---
--- @author Michael Hanus
--- @version January 2025
-------------------------------------------------------------------------

module CPM.Query.Configuration
  ( CurryEntity(..), defaultRequests, curryInfoURL
  )
 where

--- The kind of entity in a Curry module to be queried.
data CurryEntity = Operation | Type | Class | Unknown
  deriving (Eq, Show)

--- The default requests for various kinds entities.
defaultRequests :: CurryEntity -> [String]
defaultRequests cent = case cent of
  Operation  -> [ "documentation", "cass-deterministic", "cass-total"
                , "cass-terminating", "cass-demand", "failfree" ]
  Type       -> [ "documentation", "definition" ]
  Class      -> [ "definition" ]
  Unknown    -> []

--- The URL of the `curry-info` web service CGI script.
curryInfoURL :: String
curryInfoURL = "https://cpm.curry-lang.org/webapps/curry-info/run.cgi"
