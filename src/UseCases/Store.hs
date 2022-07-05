module UseCases.Store where

-- | Definition of a key/value store
data Store k v m = Store
  { listValues :: m [(k, v)],
    getValue :: k -> m (Maybe v),
    insertKeyValue :: k -> v -> m (),
    deleteValue :: k -> m ()
  }
