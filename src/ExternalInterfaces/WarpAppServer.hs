module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config               (Config (..))
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Polysemy                               (Embed, Member, Sem,
                                                         embed, interpret, runM)

-- | Warp Based implementation of AppServer
runWarpAppServer :: (Member (Embed IO) r) => Int -> Sem (AppServer : r) a -> Sem r a
runWarpAppServer port = interpret $ \case
  ServeApp app -> embed $ Warp.run port app
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in Warp.run port app
