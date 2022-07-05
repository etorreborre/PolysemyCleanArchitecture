module InterfaceAdapters.Config where

-- | global application configuration
data Config = Config {
  port        :: Int     -- ^ the port where the server is listening
, backend     :: Backend -- ^ selects the persistence backend for the KV store
, dbPath      :: String  -- ^ the path to the database
, verbose     :: Bool    -- ^ True enables logging
} deriving (Eq, Show)

data Backend = SQLite | FileServer deriving (Show, Eq)
