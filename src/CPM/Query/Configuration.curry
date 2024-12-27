-------------------------------------------------------------------------
--- Configuration parameters of the CPM querying tool.
---
--- @author Michael Hanus
--- @version December 2024
-------------------------------------------------------------------------

module CPM.Query.Configuration
  ( CurryEntity(..), defaultRequests
  )
 where

--- The kind of entity in a Curry module to be queried.
data CurryEntity = Operation | Type | Class | Unknown
  deriving (Eq, Show)

-- The default requests for various kinds entities.
defaultRequests :: CurryEntity -> [String]
defaultRequests cent = case cent of
  Operation  -> [ "cass-deterministic", "cass-total"
                , "cass-terminating", "cass-demand", "failfree" ]
  Type       -> [ "definition" ]
  Class      -> [ "definition" ]
  Unknown    -> []

