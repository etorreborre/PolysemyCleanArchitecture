module Main where

import Data.Registry
import ExternalInterfaces.ApplicationAssembly
import InterfaceAdapters.Config
import Network.Wai.Handler.Warp (run)
import SwaggerUI (swagger)
import UseCases.WebApp

-- | the POSH version of the application: REST service + SwaggerUI
main :: IO ()
main = swagger

-- | the poor man's version of the application: just a REST service
simpleMain :: IO ()
simpleMain = do
  config <- loadConfig
  app <- makeApp config
  startWebApp (webApp app)
