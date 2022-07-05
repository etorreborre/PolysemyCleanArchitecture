module InterfaceAdapters.FileServerStore where

import Control.Exception
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.List (isSuffixOf)
import System.Directory (doesFileExist, listDirectory, removeFile)
import UseCases.Store

-- | Implementation of a Store backed by file
--   keys and values are serialized / deserialized by using their JSON instances
newFileServerStore :: forall k v. (Show k, Read k, FromJSON k, Show v, ToJSON v, FromJSON v) => Store k v IO
newFileServerStore = Store {..} where
  listValues :: IO [(k, v)]
  listValues = do
    allFiles <- listDirectory dataDir
    let filteredFiles = filter (isSuffixOf extension) allFiles
    resList <- mapM (\fname -> decodeFile (dataDir ++ fname)) filteredFiles
    return $ zip (map (read . reverse . drop (length extension) . reverse) filteredFiles) resList

  getValue :: k -> IO (Maybe v)
  getValue key = do
    let fileName = getPath (show key)
    fileExists <- doesFileExist fileName
    if fileExists
      then do
        x <- decodeFile (getPath (show key))
        return (Just x)
      else return Nothing

  insertKeyValue :: k -> v -> IO ()
  insertKeyValue = encodeFile . getPath . show

  deleteValue :: k -> IO ()
  deleteValue = removeFile . show

-- | compute path of data file
getPath :: String -> String
getPath file = dataDir ++ file ++ extension

dataDir :: FilePath
dataDir = ".stack-work/"

extension :: [Char]
extension = ".json"

-- | parse an entity from a json file
decodeFile :: FromJSON a => String -> IO a
decodeFile jsonFileName = do
  eitherEntity <- eitherDecodeFileStrict jsonFileName
  case eitherEntity of
    Left msg -> throw (InternalError $ "could not parse data: " ++ msg)
    Right e -> return e

-- | exceptions that may occur during persistence operations
data PersistenceException
  = EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show)

instance Exception PersistenceException
