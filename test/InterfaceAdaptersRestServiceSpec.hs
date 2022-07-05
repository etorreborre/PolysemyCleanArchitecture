{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module InterfaceAdaptersRestServiceSpec where

import Control.Monad.Except
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8 (pack)
import Data.Function ((&))
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Registry
import Data.Time.Calendar
import Domain.ReservationDomain
import ExternalInterfaces.ApplicationAssembly
import InterfaceAdapters.Config
import InterfaceAdapters.ReservationRestService
import InterfaceAdapters.SqliteStore
import InterfaceAdapters.StdoutTracer
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodDelete, methodPost)
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Test hiding (request)
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher (bodyEquals)
import UseCases.Reservations

spec :: Spec
spec =
  with makeWebApp $
    describe "Rest Service" $ do
      it "responds with 200 for a call  GET    /seats " $
        get "/seats/2020-05-02" `shouldRespondWith` "20"
      it "responds with 200 for a valid POST   /reservations" $
        postJSON "/reservations" reservationData `shouldRespondWith` 200
      it "responds with 200 for a call  GET    /reservations " $
        get "/reservations" `shouldRespondWith` expected
      it "responds with 200 for a call  GET    /reservations/2020-05-02 " $
        get "/reservations/2020-05-02" `shouldRespondWith` 200
      it "responds with 412 if a reservation can not be done on a given day" $
        (postJSON "/reservations" reservationData >> postJSON "/reservations" reservationData) `shouldRespondWith` 412
      it "responds with 200 for a valid DELETE /reservations" $
        (deleteJSON "/reservations" reservationData >> deleteJSON "/reservations" reservationData) `shouldRespondWith` 200
      it "responds with 200 for a call  GET    /seats " $
        get "/seats/2020-05-02" `shouldRespondWith` "20"

-- * HELPERS

makeWebApp :: IO Application
makeWebApp = makeWaiApplication <$> make @(IO App) testComponents

testComponents :: Registry _ _
testComponents =
  add (newSqliteStore @Int @[String]) -- force the use of the sqlite store
    <: add noTracer -- no tracing for this spec
    <: add config -- use the provided configuration instead of the default one
    <: components

expected :: ResponseMatcher
expected = ResponseMatcher {matchBody = bodyEquals (encode reservationMap), matchStatus = 200, matchHeaders = []}

reservation :: Reservation
reservation = Reservation (fromGregorian 2020 5 2) "Amelia Jones" "amjones@example.com" 10

reservationMap :: ReservationMap
reservationMap = M.fromList [(fromGregorian 2020 5 2, [reservation])]

reservationData :: LB.ByteString
reservationData = encode reservation

postJSON path = request methodPost path [(hContentType, "application/json")]

deleteJSON path = request methodDelete path [(hContentType, "application/json")]

config :: Config
config = Config {port = 8080, dbPath = "kvs-ia-test.db", backend = SQLite, verbose = False}

main :: IO ()
main = hspec spec
