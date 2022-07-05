module InterfaceAdapters.StaticConfigProvider where

import InterfaceAdapters.Config
import InterfaceAdapters.ConfigProvider
import Polysemy (Embed, Member, Sem, embed, interpret)

runSimpleConfigProvider :: (Member (Embed IO) r) => Sem (ConfigProvider : r) a -> Sem r a
runSimpleConfigProvider = interpret $ \case
  getConfig -> embed loadConfig

-- | load application config. In real life, this would load a config file or read commandline args.
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = SQLite, dbPath = "kvs.db", verbose = True}
