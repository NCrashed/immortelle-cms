module Immortelle.CMS.Server(
    immortelleCmsApp
  ) where

import Control.Monad.Except
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text, pack)
import Immortelle.CMS.API
import Immortelle.CMS.Config
import Immortelle.CMS.Monad
import Immortelle.CMS.Types
import Network.Wai
import Servant.API
import Servant.Server

-- | WAI application for server
immortelleCmsApp :: Env -> Application
immortelleCmsApp e = serve (Proxy :: Proxy ImmortelleCmsAPI) $ enter (serverMtoHandler e) immortelleCmsServer

-- | Servant API implementation for VPN master
immortelleCmsServer :: ServerT ImmortelleCmsAPI ServerM
immortelleCmsServer =
       productGet
  :<|> productPost
  :<|> productPut
  :<|> productDelete

productGet :: ProductId -> ServerM Product
productGet = undefined

productPost :: ProductCreate -> ServerM ProductId
productPost = undefined

productPut :: ProductId -> ProductPatch -> ServerM ()
productPut = undefined

productDelete :: ProductId -> ServerM ()
productDelete = undefined
