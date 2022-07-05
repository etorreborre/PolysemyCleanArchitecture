{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SwaggerUI where

import Control.Lens
import Data.Aeson (toJSON)
import Data.Swagger hiding (port)
import Domain.ReservationDomain (Reservation (..))
import ExternalInterfaces.ApplicationAssembly (App (..), makeApp, loadConfig)
import InterfaceAdapters.Config
import InterfaceAdapters.ReservationRestService (ReservationAPI, reservationAPI)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import System.Info (os)
import System.Process (createProcess, shell)

-- | Swagger spec of Model type 'Reservation'
instance ToSchema Reservation where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "a data type representing a restaurant reservation"
      & mapped . schema . example ?~ toJSON (Reservation (read "2020-05-29") "Max Muster" "mm@muster.com" 4)

-- | Swagger spec for user API.
swaggerDoc :: Swagger
swaggerDoc =
  toSwagger reservationAPI
    & info . title .~ "Reservation API"
    & info . version .~ "0.1"
    & info . description ?~ "This is an API that provides restaurant servervations"
    & info . license ?~ ("APACHE 2.0" & url ?~ URL "http://apache.org")

-- | API type with bells and whistles, i.e. schema file and swagger-ui.
type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ReservationAPI

-- | boilerplate to guide type inference
api :: Proxy API
api = Proxy

-- | Servant server for an API
server :: App -> Server API
server app =
  swaggerSchemaUIServer
    swaggerDoc
    :<|> reservationServer app

-- | 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
createSwaggerApp :: App -> Application
createSwaggerApp app = serve api (server app)

-- | start up server and launch browser on swagger UI
swagger :: IO ()
swagger = do
  config <- loadConfig
  let p = port config
  putStrLn $ "GET all reservation: http://localhost:" ++ show p ++ "/reservations"
  putStrLn $ "Swagger UI:          http://localhost:" ++ show p ++ "/swagger-ui"
  launchSiteInBrowser p -- this line will try to open a browser and direct it to the Swagger UI
  app <- makeApp config
  run p (createSwaggerApp app)

-- | convenience function that opens the swagger UI in the default web browser
launchSiteInBrowser :: Int -> IO ()
launchSiteInBrowser port = do
  case os of
    "mingw32" -> createProcess (shell $ "start " ++ url)
    "darwin" -> createProcess (shell $ "open " ++ url)
    _ -> createProcess (shell $ "xdg-open " ++ url)
  return ()
  where
    url = "http://localhost:" ++ show port ++ "/swagger-ui"
