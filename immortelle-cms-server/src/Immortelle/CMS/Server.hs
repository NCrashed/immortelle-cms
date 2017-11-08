module Immortelle.CMS.Server(
    immortelleCmsApp
  ) where

import Control.Monad.Except
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Maybe
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text, pack)
import Data.Time
import Immortelle.CMS.API
import Immortelle.CMS.Config
import Immortelle.CMS.Monad
import Immortelle.CMS.Types
import Network.HTTP.Types.Status (ok200)
import Network.Wai
import Network.Wai.Middleware.Gzip
import Servant.API
import Servant.API.Auth.Token
import Servant.Server
import Servant.Server.Auth.Token
import Servant.Utils.StaticFiles (serveDirectoryFileServer)

import qualified Data.Set as S

-- | WAI application for server
immortelleCmsApp :: Env -> Application
immortelleCmsApp e = gzip (def { gzipFiles = GzipCacheFolder . unConfigPath $ configCacheFolder}) . frontendMiddleware $ servantApp
  where
    Config{..} = envConfig e

    -- Manual response with frontend blob
    frontendMiddleware :: Middleware
    frontendMiddleware oldApp req doResp = case (configFrontendBlob, pathInfo req) of
      (Just (ConfigPath blobPath), "all.js":_) -> doResp $ responseFile ok200 [("Content-Type", "application/javascript")] blobPath Nothing
      _ -> oldApp req doResp

    servantApp = serve (Proxy :: Proxy (ImmortelleCmsAPI :<|> Raw)) $ enter (serverMtoHandler e) immortelleCmsServer
      :<|> serveDirectoryFileServer (unConfigPath configStatic)

-- | Servant API implementation for VPN master
immortelleCmsServer :: ServerT ImmortelleCmsAPI ServerM
immortelleCmsServer =
       productGet
  :<|> productPost
  :<|> productPut
  :<|> productDelete
  :<|> productList
  :<|> proxyAuthSignin
  :<|> proxyAuthSignout
  :<|> proxyAuthTouch

productGet :: ProductId -> MToken' '["product-read"] -> ServerM Product
productGet i tok = do
  runAuth $ guardAuthToken tok
  lookupProduct i

lookupProduct :: ProductId -> ServerM Product
lookupProduct i = notFound =<< runQuery (GetProduct i)

loadAuthor :: (AuthorInfo, Double) -> ServerM (Author, Double)
loadAuthor (ai, v) = (, v) <$> case ai of
  KnownAuthor code -> notFound =<< runQuery (GetAuthorByCode code)
  UnknownAuthor name -> pure $ Author name AuthorOther

productPost :: ProductCreate -> MToken' '["product-edit"] -> ServerM ProductId
productPost ProductCreate{..} tok = do
  runAuth $ guardAuthToken tok
  i <- runUpdate GenProductId
  authors <- traverse loadAuthor $ S.toList cproductAuthors
  t <- liftIO getCurrentTime
  runUpdate $ InsertProduct Product {
      productId = i
    , productName = cproductName
    , productCategory = cproductCategory
    , productPatination = cproductPatination
    , productAuthors = S.fromList authors
    , productIncrustations = cproductIncrustations
    , productPrice = cproductPrice
    , productCreation = cproductCreation
    , productLocation = cproductLocation
    , productBooked = cproductBooked
    , productInGroup = cproductInGroup
    , productTimestamp = t
    }
  pure i

productPut :: ProductId -> ProductPatch -> MToken' '["product-edit"] -> ServerM ()
productPut i ProductPatch{..} tok = do
  runAuth $ guardAuthToken tok
  p <- lookupProduct i
  authors <- traverse loadAuthor $ S.toList pproductAuthors
  runUpdate $ InsertProduct p {
      productName = pproductName
    , productCategory = pproductCategory
    , productPatination = pproductPatination
    , productAuthors = S.fromList authors
    , productIncrustations = pproductIncrustations
    , productPrice = pproductPrice
    , productCreation = pproductCreation
    , productLocation = pproductLocation
    , productBooked = pproductBooked
    , productInGroup = pproductInGroup
    }

productDelete :: ProductId -> MToken' '["product-edit"] -> ServerM ()
productDelete i tok = do
  runAuth $ guardAuthToken tok
  runUpdate $ DeleteProduct i

productList :: Maybe Text -> Maybe PageInfo -> MToken' '["product-read"] -> ServerM (PagedList Product)
productList mt pinfo tok = do
  runAuth $ guardAuthToken tok
  runQuery $ ListProducts mt pinfo

proxyAuthSignin :: Maybe Login -> Maybe Password -> Maybe Seconds -> ServerM (OnlyField "token" SimpleToken)
proxyAuthSignin mlogin mpass mseconds = runAuth $ authSignin mlogin mpass mseconds

proxyAuthSignout :: MToken' '[] -> ServerM Unit
proxyAuthSignout tok = runAuth $ authSignout tok

proxyAuthTouch :: Maybe Seconds -> MToken' '[] -> ServerM Unit
proxyAuthTouch mseconds tok = runAuth $ authTouch mseconds tok
