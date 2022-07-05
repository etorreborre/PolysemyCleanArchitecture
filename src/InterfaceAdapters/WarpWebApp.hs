module InterfaceAdapters.WarpWebApp where

import InterfaceAdapters.Config
import InterfaceAdapters.ReservationRestService
import Network.Wai.Handler.Warp (run)
import Servant.Server
import UseCases.Tracer
import UseCases.WebApp

-- | Create and start a WAI Application
newWarpWebApp :: Config -> Tracer IO -> ServerT ReservationAPI Handler -> WebApp
newWarpWebApp config tracer api = WebApp {..} where
  startWebApp :: IO ()
  startWebApp = do
    let p = port config
    trace tracer $ "Starting server on port " ++ show p
    run p (serve reservationAPI api)
