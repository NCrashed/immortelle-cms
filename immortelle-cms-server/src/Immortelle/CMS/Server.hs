module Immortelle.CMS.Server(
    immortelleCmsApp
  ) where

import Control.Monad.Except
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
import Servant.Server
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

productGet :: ProductId -> ServerM Product
productGet i = notFound =<< runQuery (GetProduct i)

loadAuthor :: AuthorInfo -> ServerM Author
loadAuthor ai = case ai of
  KnownAuthor code -> notFound =<< runQuery (GetAuthorByCode code)
  UnknownAuthor name -> pure $ Author name AuthorOther

productPost :: ProductCreate -> ServerM ProductId
productPost ProductCreate{..} = do
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

productPut :: ProductId -> ProductPatch -> ServerM ()
productPut i ProductPatch{..} = do
  p <- productGet i
  authors <- traverse loadAuthor $ S.toList pproductAuthors
  runUpdate $ InsertProduct p {
      productCategory = pproductCategory
    , productPatination = pproductPatination
    , productAuthors = S.fromList authors
    , productIncrustations = pproductIncrustations
    }

productDelete :: ProductId -> ServerM ()
productDelete i = runUpdate $ DeleteProduct i
