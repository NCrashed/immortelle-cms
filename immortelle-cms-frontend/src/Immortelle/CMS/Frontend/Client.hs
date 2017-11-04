module Immortelle.CMS.Frontend.Client(
  -- * API
  -- ** Product
    getProduct
  , insertProduct
  , updateProduct
  , deleteProduct
  -- ** Auth
  , authSignin
  , authSignout
  , authTouch
  -- * Helpers
  , textifyResult
  , dangerResult
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson.WithField
import Data.Aeson.Unit
import Data.Bifunctor
import Data.ByteString
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text
import GHC.TypeLits
import Reflex.Dom
import Servant.API
import Servant.API.Auth.Token
import Servant.Reflex

import Immortelle.CMS.API
import Immortelle.CMS.Frontend.Monad
import Immortelle.CMS.Frontend.Utils
import Immortelle.CMS.Types

import Web.Reflex.Bootstrap hiding (PagedList(..))
import qualified Web.Reflex.Bootstrap as B

-- | Shortcut for authorisation header
type DToken t (perms :: [Symbol]) = Dynamic t (Either Text (Token' perms))
-- | Shortcut for request body
type DBody t a = Dynamic t (Either Text a)
-- | Shortcut for request capture clause
type DCapture t a = DBody t a
-- | Shortcut for query body
type DParam t a = Dynamic t (QParam a)
-- | Shortcut for endpoint ending
type DEndpoint t m a =  Event t () -> m (Event t (ReqResult () a))
-- | Shortcut for whole RPC endpoint
type DRPC t m (ps :: [Symbol]) a b = DBody t a -> DToken t ps -> DEndpoint t m b

-- | Hold endpoints in named sockets
data ServerClient t m = ServerClient {
  getProductMethod      :: DCapture t ProductId                           -> DToken t '["product-read"] -> DEndpoint t m Product
, insertProductMethod   :: DBody t ProductCreate                          -> DToken t '["product-edit"] -> DEndpoint t m ProductId
, updateProductMethod   :: DCapture t ProductId  -> DBody t ProductPatch  -> DToken t '["product-edit"] -> DEndpoint t m ()
, deleteProductMethod   :: DCapture t ProductId                           -> DToken t '["product-edit"] -> DEndpoint t m ()
, authSigninMethod      :: DParam t Login -> Dynamic t (QParam Password) -> Dynamic t (QParam Seconds) -> DEndpoint t m (OnlyField "token" SimpleToken)
, authSignoutMethod     :: DToken t '[] -> DEndpoint t m Unit
, authTouchMethod       :: DParam t Seconds -> DToken t '[] -> DEndpoint t m Unit
}

-- | Collect client endpoints into record. Noinline pragma is used to calculate
-- the struct only once.
serverEndpoints :: forall t m . MonadWidget t m => Proxy m -> ServerClient t m
{-# NOINLINE serverEndpoints #-}
serverEndpoints pm = ServerClient {..}
  where
    (      getProductMethod
      :<|> insertProductMethod
      :<|> updateProductMethod
      :<|> deleteProductMethod
      :<|> authSigninMethod
      :<|> authSignoutMethod
      :<|> authTouchMethod
      ) = client (Proxy :: Proxy ImmortelleCmsAPI) pm (Proxy :: Proxy ()) (pure (BasePath "/"))

-- | Convert req result to text
textifyResult :: ReqResult () a -> Either Text a
textifyResult r = case r of
  ResponseSuccess _ a _ -> Right a
  ResponseFailure _ e _ -> Left e
  RequestFailure _ e -> Left e

-- | Display error in danger panel
dangerResult :: MonadWidget t m => Event t (ReqResult () a) -> m (Event t a)
dangerResult = handleDanger . fmap textifyResult

instance Functor QParam where
  fmap f v = case v of
    QParamSome a -> QParamSome (f a)
    QNone -> QNone
    QParamInvalid e -> QParamInvalid e

-- | Wrap RPC endpoint to use with simple event input.
wrapEndpoint :: forall t m a b ps . MonadWidget t m
  => Proxy ps -- ^ Fix permissions ambigous type
  -> DRPC t m ps a b -- ^ Raw endpoint
  -> Dynamic t SimpleToken -- ^ Authorisation token
  -> Event t a -- ^ Input data
  -> m (Event t (ReqResult () b)) -- ^ Result
wrapEndpoint _ endpoint tokenD bodyE = do
  let tokenD' = Right . Token <$> tokenD :: Dynamic t (Either Text (Token' ps))
  bodyD <- holdDyn (Left "") (Right <$> bodyE)
  endpoint bodyD tokenD' (void $ updated bodyD)

-- | Transform from API paged list to frontend paged list
transPagedList :: PagedList i a -> B.PagedList a
transPagedList l = B.PagedList {
    B.pagedListItems = (\(WithField _ a) -> a) <$> pagedListItems l
  , B.pagedListTotal = pagedListPages l
  }

-- | Convinient helper of 'transPagedList'
transPagedList' :: (Functor m, Reflex t) => m (Event t (ReqResult () (PagedList i a))) -> m (Event t (ReqResult () (B.PagedList a)))
transPagedList' = fmap . fmap . fmap $ transPagedList

-- | Query server about product by id
getProduct :: forall t m . MonadFront t m => Event t ProductId -> m (Event t (ReqResult () Product))
getProduct bodyE = do
  tokd <- getAuthToken
  bodyD <- holdDyn (Left "") (Right <$> bodyE)
  endpoint bodyD (Right . Token <$> tokd) (void $ updated bodyD)
  where
    endpoint = getProductMethod $ serverEndpoints (Proxy :: Proxy m)

-- | Insert new product on server
insertProduct :: forall t m . MonadFront t m => Event t ProductCreate -> m (Event t (ReqResult () ProductId))
insertProduct bodyE = do
  tokd <- getAuthToken
  bodyD <- holdDyn (Left "") (Right <$> bodyE)
  endpoint bodyD (Right . Token <$> tokd) (void $ updated bodyD)
  where
    endpoint = insertProductMethod $ serverEndpoints (Proxy :: Proxy m)

-- | Update product on server
updateProduct :: forall t m . MonadFront t m => Event t (ProductId, ProductPatch) -> m (Event t (ReqResult () ()))
updateProduct bodyE = do
  tokd <- getAuthToken
  bodyD <- holdDyn (Left "") (Right <$> bodyE)
  let pidD = fmap fst <$> bodyD
      pcrD = fmap snd <$> bodyD
  endpoint pidD pcrD (Right . Token <$> tokd) (void $ updated bodyD)
  where
    endpoint = updateProductMethod $ serverEndpoints (Proxy :: Proxy m)

-- | Delete product on server
deleteProduct :: forall t m . MonadFront t m => Event t ProductId -> m (Event t (ReqResult () ()))
deleteProduct bodyE = do
  tokd <- getAuthToken
  bodyD <- holdDyn (Left "") (Right <$> bodyE)
  endpoint bodyD (Right . Token <$> tokd) (void $ updated bodyD)
  where
    endpoint = deleteProductMethod $ serverEndpoints (Proxy :: Proxy m)

-- | Login and get authorisation token
authSignin :: forall t m a . MonadWidget t m => Event t (Login, Password, Seconds) -> m (Event t (ReqResult () SimpleToken))
authSignin bodyE = do
  bodyD <- holdDyn QNone (QParamSome <$> bodyE)
  let
    loginD = fmap (\(a, _, _) -> a) <$> bodyD
    passD  = fmap (\(_, a, _) -> a) <$> bodyD
    secsD  = fmap (\(_, _, a) -> a) <$> bodyD
  resE <- endpoint loginD passD secsD (void $ updated bodyD)
  return $ fmap (\(OnlyField v) -> v) <$> resE
  where
    endpoint = authSigninMethod $ serverEndpoints (Proxy :: Proxy m)

-- | Logout from server
authSignout :: forall t m a . MonadWidget t m => Event t SimpleToken -> m (Event t (ReqResult () ()))
authSignout bodyE = do
  bodyD <- holdDyn (Left "") (Right . Token <$> bodyE)
  resE <- endpoint bodyD (void $ updated bodyD)
  return $ void <$> resE
  where
    endpoint = authSignoutMethod $ serverEndpoints (Proxy :: Proxy m)

-- | Prolong authorisation token for given amounts of seconds
authTouch :: forall t m a . MonadWidget t m => Event t (Seconds, SimpleToken) -> m (Event t (ReqResult () ()))
authTouch bodyE = do
  bodyD <- holdDyn (Left "") ((\(s, t) -> Right (s, Token t)) <$> bodyE)
  let secondsD = ffor bodyD $ \res -> case res of
        Left _ -> QNone
        Right (s, _) -> QParamSome s
      tokenD = fmap snd <$> bodyD
  resE <- endpoint secondsD tokenD (void $ updated bodyD)
  return $ void <$> resE
  where
    endpoint = authTouchMethod $ serverEndpoints (Proxy :: Proxy m)
