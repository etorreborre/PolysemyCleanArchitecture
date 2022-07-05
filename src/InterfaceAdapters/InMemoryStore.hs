module InterfaceAdapters.InMemoryStore where

import Data.IORef
import Safe
import UseCases.Store

-- | Implementation of a Store using an in-memory map
newInMemoryStore :: forall k v. (Eq k) => IO (Store k v IO)
newInMemoryStore = newInMemoryStore_ <$> newIORef []

newInMemoryStore_ :: forall k v. (Eq k) => IORef [(k, v)] -> Store k v IO
newInMemoryStore_ ref = Store {..} where
  listValues :: IO [(k, v)]
  listValues = readIORef ref

  getValue :: k -> IO (Maybe v)
  getValue key = do
    kvs <- readIORef ref
    pure . headMay $ snd <$> dropWhile (\(k, _) -> k /= key) kvs

  insertKeyValue :: k -> v -> IO ()
  insertKeyValue k v = modifyIORef ref ((k, v) :)

  deleteValue :: k -> IO ()
  deleteValue key = modifyIORef ref (filter (\(k, _) -> k /= key))
