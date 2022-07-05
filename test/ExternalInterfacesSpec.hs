{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module ExternalInterfacesSpec where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import Data.Registry
import Data.Time (fromGregorian)
import Domain.ReservationDomain
import ExternalInterfaces.ApplicationAssembly
import InterfaceAdapters.Config
import InterfaceAdapters.StdoutTracer
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher (bodyEquals)
import UseCases.WebApp

spec :: Spec
spec = do
  it "can load the app config" $ do
    conf <- loadConfig
    port conf `shouldBe` 8080
    backend conf `shouldBe` SQLite
    dbPath conf `shouldBe` "kvs.db"
    verbose conf `shouldBe` True

  with (makeWebApp (config {verbose = False, backend = FileServer})) $
    describe "Service with disabled tracing" $ do
      it "responds with 20 for a first call to GET /seats/YYYY-MM-DD" $
        get "/seats/2020-05-03" `shouldRespondWith` "20"

  with (makeWebApp config) $
    describe "Rest Service" $ do
      it "responds with 20 for a first call to GET /seats/YYYY-MM-DD" $
        get "/seats/2020-05-02" `shouldRespondWith` "20"

      it "responds with 200 for a valid POST /reservations" $
        postJSON "/reservations" reservationData `shouldRespondWith` 200

      it "responds with 200 for a call GET /reservations " $
        get "/reservations" `shouldRespondWith` expected

      it "responds with 412 if a reservation can not be done on a given day" $
        (postJSON "/reservations" reservationData >> postJSON "/reservations" reservationData) `shouldRespondWith` 412

      it "responds to call to GET /seats/YYYY-MM-DD with correct number when seats are already booked" $
        get "/seats/2020-05-02" `shouldRespondWith` "8"

      it "responds with 200 for a valid DELETE /reservations" $
        deleteJSON "/reservations" reservationData `shouldRespondWith` 200

-- * HELPERS

makeWebApp :: Config -> IO Application
makeWebApp c = makeWaiApplication <$> make @(IO App) (testComponents c)

testComponents :: Config -> Registry _ _
testComponents config =
  add noTracer -- no tracing for this spec
    <: add config -- use the provided configuration instead of the default one
    <: components

expected :: ResponseMatcher
expected = ResponseMatcher {matchBody = bodyEquals (encode reservationMap), matchStatus = 200, matchHeaders = []}

reservation :: Reservation
reservation = Reservation (fromGregorian 2020 5 2) "Amelia Jones" "amjones@example.com" 12

reservationMap :: ReservationMap
reservationMap = M.fromList [(fromGregorian 2020 5 2, [reservation])]

reservationData :: LB.ByteString
reservationData = encode reservation

postJSON path = request methodPost path [(hContentType, "application/json")]

deleteJSON path = request methodDelete path [(hContentType, "application/json")]

main :: IO ()
main = hspec spec

config :: Config
config = Config {port = 8080, backend = SQLite, dbPath = "kvs-assembly.db", verbose = False}
