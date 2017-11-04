module Immortelle.CMS.Monad(
    Env
  , newEnv
  , ServerM
  , runServerM
  , runServerMIO
  , serverMtoHandler
  , getConfig
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Monoid
import Servant.Server
import Immortelle.CMS.Config

data Env = Env {
  envConfig :: !Config
}

newEnv :: MonadIO m => Config -> m Env
newEnv cfg = pure Env {
    envConfig = cfg
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
