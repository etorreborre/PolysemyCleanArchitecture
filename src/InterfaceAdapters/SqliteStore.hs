{-# LANGUAGE OverloadedStrings #-}

module InterfaceAdapters.SqliteStore where

import Data.Aeson (decode, encode)
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import InterfaceAdapters.Config
import UseCases.Store
import UseCases.Tracer

-- | Implementation of a Store using Sqlite
--   Keys and values are stored as text by encoding/decoding them using their JSON instances
newSqliteStore :: forall k v. (Show k, Read k, ToJSON v, FromJSON v) => Config -> Tracer IO -> Store k v IO
newSqliteStore config tracer = Store {..} where
  listValues :: IO [(k, v)]
  listValues = do
    trace tracer "listAction:"
    connection <- getConnection
    rows <- SQL.query_ connection "SELECT key, value FROM store" :: IO [KeyValueRow]
    let maybeList = map toKV rows
    return $ catNestedMaybe maybeList
    where
      toKV (KeyValueRow key value) = ((read . T.unpack) key, (decode . encodeUtf8) value)
      catNestedMaybe [] = []
      catNestedMaybe ((key, Just value) : xs) = (key, value) : catNestedMaybe xs
      catNestedMaybe ((_, Nothing) : xs) = catNestedMaybe xs

  getValue :: k -> IO (Maybe v)
  getValue key = do
    trace tracer $ "getAction: " ++ show key
    connection <- getConnection
    rows <- SQL.queryNamed connection "SELECT key, value FROM store WHERE key = :key" [":key" := show key] :: IO [KeyValueRow]
    case rows of
      [] -> return Nothing
      (KeyValueRow _key value) : _ -> return $ (decode . encodeUtf8) value

  insertKeyValue :: k -> v -> IO ()
  insertKeyValue key value = do
    trace tracer $ "insertAction: " ++ show key ++ " " ++ show (encode value)
    let (query, params) =
          ( "INSERT INTO store (key, value) VALUES (:key, :value) "
              <> "ON CONFLICT (key) DO UPDATE SET value = excluded.value",
            [":key" := show key, ":value" := encodedValue]
          )
          where
            encodedValue = (decodeUtf8 . encode) value
    connection <- getConnection
    SQL.executeNamed connection query params

  deleteValue :: k -> IO ()
  deleteValue key = do
    trace tracer $ "deleteAction: " ++ show key
    connection <- getConnection
    SQL.executeNamed connection "DELETE FROM store WHERE key = :key" [":key" := show key]

  getConnection :: IO SQL.Connection
  getConnection = do
    trace tracer $ "open connection to: " ++ dbPath config
    connection <- SQL.open $ dbPath config
    SQL.execute_ connection "CREATE TABLE IF NOT EXISTS store (key TEXT PRIMARY KEY, value TEXT)"
    return connection

data KeyValueRow = KeyValueRow T.Text T.Text
  deriving (Show)

instance FromRow KeyValueRow where
  fromRow = KeyValueRow <$> field <*> field
