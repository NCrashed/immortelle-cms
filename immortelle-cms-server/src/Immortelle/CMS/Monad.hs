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
  , AuthM(..)
  , runAuth
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
import Servant.Server.Auth.Token.Acid as A
import Servant.Server.Auth.Token.Config
import Servant.Server.Auth.Token.Model

data Env = Env {
  envConfig     :: !Config
, envDb         :: !(AcidState DB)
, envAuthConfig :: !AuthConfig
}

newEnv :: MonadIO m => Config -> m Env
newEnv cfg = do
  db <- liftIO $ openLocalStateFrom (unConfigPath $ configState cfg) emptyDB
  _ <- runAcidBackendT defaultAuthConfig db $ ensureAdmin 17 "admin" (configAdminPassword cfg) "admin@localhost"
  pure Env {
      envConfig     = cfg
    , envDb         = db
    , envAuthConfig = defaultAuthConfig
    }

newtype ServerM a = ServerM { unServerM :: ReaderT Env (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadError ServantErr)

-- | Lift servant monad to server monad
liftHandler :: Handler a -> ServerM a
liftHandler = ServerM . lift . lift

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

-- Derive HasStorage for 'AcidBackendT' with your 'DB'
deriveAcidHasStorage ''DB

-- | Special monad for authorisation actions
newtype AuthM a = AuthM { unAuthM :: AcidBackendT DB IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, HasAuthConfig, HasStorage)

-- | Execution of authorisation actions that require 'AuthHandler' context
runAuth :: AuthM a -> ServerM a
runAuth m = do
  cfg <- ServerM $ asks envAuthConfig
  db <- ServerM $ asks envDb
  liftHandler $ Handler. ExceptT $ runAcidBackendT cfg db $ unAuthM m
