module Immortelle.CMS.Frontend.Monad(
    MonadFront
  , HasAuthToken(..)
  ) where

import Control.Monad.Reader
import Reflex
import Reflex.Dom
import Servant.API.Auth.Token

class HasAuthToken t m where
  getAuthToken :: m (Dynamic t SimpleToken)

instance Monad m => HasAuthToken t (ReaderT (Dynamic t SimpleToken) m) where
  getAuthToken = ask
  {-# INLINE getAuthToken #-}

type MonadFront t m = (HasAuthToken t m, MonadWidget t m)
