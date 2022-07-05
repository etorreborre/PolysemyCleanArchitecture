{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module ExternalInterfaces.ApplicationAssembly where

import Data.Registry
import Data.Registry.Internal.Types
import Data.Time.Calendar (Day)
import Data.Typeable
import Domain.ReservationDomain
import InterfaceAdapters.Config
import InterfaceAdapters.FileServerStore
import InterfaceAdapters.ReservationRestService
import InterfaceAdapters.SqliteStore
import InterfaceAdapters.StdoutTracer
import InterfaceAdapters.WarpWebApp
import Network.Wai.Handler.Warp (run)
import Servant.Server
import UseCases.Reservations
import UseCases.Store
import UseCases.Tracer
import UseCases.WebApp

data App = App
  { configuration :: Config,
    tracer :: Tracer IO,
    store :: Store Day [Reservation] IO,
    reservations :: Reservations IO,
    reservationApi :: ReservationApi,
    reservationServer :: ServerT ReservationAPI Handler,
    webApp :: WebApp
  }

makeApp :: Config -> IO App
makeApp config = make @(IO App) (add config <: components)

makeWaiApplication :: App -> Application
makeWaiApplication App {..} = serve reservationAPI reservationServer

components :: Registry _ _
components =
  add App
    <: add newWarpWebApp
    <: add newReservationServer
    <: add newReservationApi
    <: add (newReservations @IO)
    <: add newBackendStore
    <: add newTracer
    <: add backend
    <: add productionConfig

newBackendStore :: Config -> Tracer IO -> Backend -> Store Day [Reservation] IO
newBackendStore config tracer SQLite = newSqliteStore config tracer
newBackendStore _ tracer FileServer = newFileServerStore

-- | load application config. In real life, this would load a config file or read commandline args.
loadConfig :: IO Config
loadConfig = return productionConfig

productionConfig :: Config
productionConfig = Config {port = 8080, backend = SQLite, dbPath = "kvs.db", verbose = True}

-- | Registry shortcut
add :: forall a b. (ApplyVariadic IO a b, Typeable a, Typeable b) => a -> Typed b
add = funTo @IO
