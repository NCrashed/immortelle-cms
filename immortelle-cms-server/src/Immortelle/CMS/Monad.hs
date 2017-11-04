module Immortelle.CMS.Monad(
    Env(..)
  , newEnv
  , ServerM
  , runServerM
  , runServerMIO
  , serverMtoHandler
  , getConfig
  , runUpdate
  , runQuery
  , module Immortelle.CMS.State
  , notFound
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Acid
import Data.Monoid
import Immortelle.CMS.Config
import Immortelle.CMS.State
import Servant.Server

data Env = Env {
  envConfig :: !Config
, envDb     :: !(AcidState DB)
}

newEnv :: MonadIO m => Config -> m Env
newEnv cfg = do
  db <- liftIO $ openLocalStateFrom (unConfigPath $ configState cfg) emptyDB
  pure Env {
      envConfig = cfg
    , envDb     = db
    }

newtype ServerM a = ServerM { unServerM :: ReaderT Env (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadError ServantErr)

-- | Run server monad in servant handler
runServerM :: Env -> ServerM a -> Handler a
runServerM e = runStdoutLoggingT . flip runReaderT e . unServerM

-- | Execution of 'ServerM' in IO monad
runServerMIO :: Env -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

-- | Transformation to Servant 'Handler'
serverMtoHandler :: Env -> ServerM :~> Handler
serverMtoHandler e = NT (runServerM e)

-- | Getting server configuration
getConfig :: ServerM Config
getConfig = ServerM $ asks envConfig

-- | Execute update query in DB
runUpdate :: (UpdateEvent event, EventState event ~ DB) => event -> ServerM (EventResult event)
runUpdate e = do
  db <- ServerM $ asks envDb
  liftIO $ update db e

-- | Execute select query in DB
runQuery :: (QueryEvent event, EventState event ~ DB) => event -> ServerM (EventResult event)
runQuery e = do
  db <- ServerM $ asks envDb
  liftIO $ query db e

-- | Wrap 'Nothing' to '404' http error
notFound :: Maybe a -> ServerM a
notFound ma = case ma of
  Nothing -> throwError err404 { errBody = "Cannot find record" }
  Just a -> pure a
