{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module InterfaceAdaptersStoreSpec where

import Data.Functor
import Data.Registry
import ExternalInterfaces.ApplicationAssembly
import InterfaceAdapters.Config
import InterfaceAdapters.InMemoryStore
import InterfaceAdapters.SqliteStore
import InterfaceAdapters.StdoutTracer
import Test.Hspec
import UseCases.Store

spec :: Spec
spec =
  describe "The Store implementations" $ do
    describe "return Nothing if nothing can be found for a given key" $
      withStore $ \store -> do
        maybeMatch <- getValue store key
        maybeMatch `shouldBe` Nothing

    describe "can persist a key-value pair" $ do
      withStore $ \store -> do
        insertKeyValue store key value
        maybeMatch <- getValue store key
        maybeMatch `shouldBe` Just value

    describe "can return a list of all key-value entries" $ do
      withStore $ \store -> do
        values <- listValues store
        length values `shouldBe` 1

    describe "delete a value by key" $ do
      withStore $ \store -> do
        deleteValue store key
        maybeMatch <- getValue store key
        maybeMatch `shouldBe` Nothing

-- HELPERS

withStore :: (Store Int [String] IO -> Expectation) -> Spec
withStore f = do
  describe "with the in-memory store" $
    -- override the current registry with an in-memory store instead
    void . runIO $ f <$> makeStore (add (newInMemoryStore @Int @[String]) <: testComponents)
  describe "with the sqlite store" $ do
    void . runIO $ f <$> makeStore testComponents

makeStore :: Registry _ _ -> IO (Store Int [String] IO)
makeStore = make

config :: Config
config = Config {port = 8080, dbPath = "kvs-test.db", backend = SQLite, verbose = False}

testComponents :: Registry _ _
testComponents =
  add (newSqliteStore @Int @[String]) -- add a constructor for the sqlite store
    <: add noTracer -- no tracing for this spec
    <: add config -- use the provided configuration instead of the default one

key :: Int
key = 4711

value :: [String]
value = ["In the morning", "I don't drink coffee", "But lots of curcuma chai."]

main :: IO ()
main = hspec spec
