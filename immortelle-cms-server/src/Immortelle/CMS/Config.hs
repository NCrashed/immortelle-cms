module Immortelle.CMS.Config(
    Config(..)
  , ConfigPath(..)
  , absolutize
  , loadConfig
  ) where

import Control.Monad.IO.Class
import Data.Aeson hiding (defaultOptions)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.String
import Data.Text (Text)
import Data.Yaml.Config
import GHC.Generics
import System.Directory
import System.FilePath

import Immortelle.CMS.Aeson

-- | Define path to a file relative to the config
newtype ConfigPath = ConfigPath { unConfigPath :: FilePath }
  deriving (Eq, Show, Generic, Data, FromJSON, ToJSON)

instance IsString ConfigPath where
  fromString = ConfigPath

-- | Transform all 'SitePath' to absolute path with given prefix
absolutize :: (Uniplate a, Data a, MonadIO m) => FilePath -> a -> m a
absolutize prefix a =  liftIO $ do
  p <- canonicalizePath prefix
  transformBiM (mkAbs p) a
  where
    mkAbs :: FilePath -> ConfigPath -> IO ConfigPath
    mkAbs pref (ConfigPath p) = pure . ConfigPath $ if isAbsolute p then p else pref </> p

data Config = Config {
  configHost            :: !Text
, configPort            :: !Int
, configStatic          :: !ConfigPath
, configFrontendBlob    :: !(Maybe ConfigPath)
, configDetailedLogging :: !Bool
, configState           :: !ConfigPath
, configAdminPassword   :: !Text
, configCacheFolder     :: !ConfigPath
} deriving (Generic, Data)
deriveJSON defaultOptions ''Config

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = do
  cfg <- liftIO $ loadYamlSettings [path] [] useEnv
  absolutize (takeDirectory path) cfg
