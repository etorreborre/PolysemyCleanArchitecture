{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module UseCaseSpec where

import Control.Monad.Cont (liftIO)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Registry
import Data.Time.Calendar
import Domain.ReservationDomain
import ExternalInterfaces.ApplicationAssembly
import InterfaceAdapters.FileServerStore
import InterfaceAdapters.StdoutTracer
import System.Directory (listDirectory, removeFile)
import Test.Hspec
import UseCases.Reservations

spec :: Spec
spec = do
  runIO deleteAllFiles
  reservations <- runIO makeReservations
  describe "Reservation Use Case (with file IO)" $ do
    it "needs a file cleaning for repeatable tests in the file system..." $ do
      map <- listAll reservations
      M.size map `shouldBe` 0

    it "returns Nothing if there are no reservations for a given day" $ do
      result <- fetch reservations day
      result `shouldBe` []

    it "can add a reservation if there are enough free seats" $ do
      let goodReservation = head res
      tryReservation reservations goodReservation
      map <- listAll reservations
      M.size map `shouldBe` 1
      rs <- fetch reservations day
      goodReservation `elem` rs `shouldBe` True

    it "fetches a list of reservations from the KV store" $ do
      result <- fetch reservations day
      result `shouldBe` res

    it "can retrieve a map of all reservations" $ do
      map <- listAll reservations
      M.size map `shouldBe` 1

    it "throws an error if a reservation is not possible" $ do
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      result <- tryReservation reservations badReservation
      result `shouldBe` Left (ReservationNotPossible ("Sorry, only 16 seats left on " ++ show day))

    it "can cancel a reservation" $ do
      let res1 = head res
      tryReservation reservations res1
      rs <- fetch reservations day
      length rs `shouldBe` 2

      cancel reservations res1

      rs' <- fetch reservations day
      length rs' `shouldBe` 1

-- * HELPERS

makeReservations :: IO (Reservations IO)
makeReservations = make @(IO (Reservations IO)) testComponents

testComponents :: Registry _ _
testComponents =
  add (newFileServerStore @Day @[Reservation]) -- force the file server to be used
    <: add noTracer -- no tracing for this spec
    <: components

-- | helper function to clean the test data files
deleteAllFiles :: IO [()]
deleteAllFiles = do
  allFiles <- liftIO $ listDirectory dataDir
  let filteredFiles = filter (isSuffixOf ".json") allFiles
  mapM (liftIO . removeFile . (dataDir ++)) filteredFiles

day :: Day
day = fromGregorian 2020 5 2

res :: [Reservation]
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

main :: IO ()
main = hspec spec
