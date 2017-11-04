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
import Immortelle.CMS.API
import Immortelle.CMS.Config
import Immortelle.CMS.Monad
import Immortelle.CMS.Types
import Network.HTTP.Types.Status (ok200)
import Network.Wai
import Servant.API
import Servant.API.Auth.Token
import Servant.Server
import Servant.Server.Auth.Token
import Servant.Utils.StaticFiles (serveDirectoryFileServer)

import qualified Data.Set as S

-- | WAI application for server
immortelleCmsApp :: Env -> Application
immortelleCmsApp e = frontendMiddleware servantApp
  where
    -- Manual response with frontend blob
    frontendMiddleware :: Middleware
    frontendMiddleware oldApp req doResp = case (configFrontendBlob . envConfig $ e, pathInfo req) of
      (Just (ConfigPath blobPath), "all.js":_) -> doResp $ responseFile ok200 [] blobPath Nothing
      _ -> oldApp req doResp

    servantApp = serve (Proxy :: Proxy (ImmortelleCmsAPI :<|> Raw)) $ enter (serverMtoHandler e) immortelleCmsServer
      :<|> serveDirectoryFileServer (unConfigPath . configStatic . envConfig $ e)

-- | Servant API implementation for VPN master
immortelleCmsServer :: ServerT ImmortelleCmsAPI ServerM
immortelleCmsServer =
       productGet
  :<|> productPost
  :<|> productPut
  :<|> productDelete
  :<|> proxyAuthSignin
  :<|> proxyAuthSignout
  :<|> proxyAuthTouch

productGet :: ProductId -> MToken' '["product-read"] -> ServerM Product
productGet i tok = do
  runAuth $ guardAuthToken tok
  lookupProduct i

lookupProduct :: ProductId -> ServerM Product
lookupProduct i = notFound =<< runQuery (GetProduct i)

loadAuthor :: AuthorInfo -> ServerM Author
loadAuthor ai = case ai of
  KnownAuthor code -> notFound =<< runQuery (GetAuthorByCode code)
  UnknownAuthor name -> pure $ Author name AuthorOther

productPost :: ProductCreate -> MToken' '["product-edit"] -> ServerM ProductId
productPost ProductCreate{..} tok = do
  runAuth $ guardAuthToken tok
  i <- runUpdate GenProductId
  authors <- traverse loadAuthor $ S.toList cproductAuthors
  runUpdate $ InsertProduct Product {
      productId = i
    , productCategory = cproductCategory
    , productPatination = cproductPatination
    , productAuthors = S.fromList authors
    , productIncrustations = cproductIncrustations
    }
  pure i

productPut :: ProductId -> ProductPatch -> MToken' '["product-edit"] -> ServerM ()
productPut i ProductPatch{..} tok = do
  runAuth $ guardAuthToken tok
  p <- lookupProduct i
  authors <- traverse loadAuthor $ S.toList pproductAuthors
  runUpdate $ InsertProduct p {
      productCategory = pproductCategory
    , productPatination = pproductPatination
    , productAuthors = S.fromList authors
    , productIncrustations = pproductIncrustations
    }

productDelete :: ProductId -> MToken' '["product-edit"] -> ServerM ()
productDelete i tok = do
  runAuth $ guardAuthToken tok
  runUpdate $ DeleteProduct i

proxyAuthSignin :: Maybe Login -> Maybe Password -> Maybe Seconds -> ServerM (OnlyField "token" SimpleToken)
proxyAuthSignin mlogin mpass mseconds = runAuth $ authSignin mlogin mpass mseconds

proxyAuthSignout :: MToken' '[] -> ServerM Unit
proxyAuthSignout tok = runAuth $ authSignout tok

proxyAuthTouch :: Maybe Seconds -> MToken' '[] -> ServerM Unit
proxyAuthTouch mseconds tok = runAuth $ authTouch mseconds tok
