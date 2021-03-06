module Immortelle.CMS.API(
    ImmortelleCmsAPI
  , AuthorInfo(..)
  , ProductCreate(..)
  , ProductPatch(..)
  , module Immortelle.CMS.Pagination
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Immortelle.CMS.Aeson
import Immortelle.CMS.Pagination
import Immortelle.CMS.Types
import Servant.API
import Servant.API.Auth.Token

data AuthorInfo = KnownAuthor AuthorCode | UnknownAuthor Text
  deriving (Eq, Ord, Show, Generic)
deriveJSON defaultOptions ''AuthorInfo

data ProductCreate = ProductCreate {
  cproductName          :: Text
, cproductCategory      :: ProductCategoryData
, cproductPatination    :: Maybe Patination
, cproductAuthors       :: Set (AuthorInfo, Double)
, cproductIncrustations :: Set IncrustationData
, cproductPrice         :: Price
, cproductCreation      :: Maybe Day
, cproductLocation      :: Maybe Text
, cproductBooked        :: Maybe Text
, cproductInGroup       :: Bool
} deriving (Show, Generic)
deriveJSON defaultOptions ''ProductCreate

data ProductPatch = ProductPatch {
  pproductName          :: Text
, pproductCategory      :: ProductCategoryData
, pproductPatination    :: Maybe Patination
, pproductAuthors       :: Set (AuthorInfo, Double)
, pproductIncrustations :: Set IncrustationData
, pproductPrice         :: Price
, pproductCreation      :: Maybe Day
, pproductLocation      :: Maybe Text
, pproductBooked        :: Maybe Text
, pproductInGroup       :: Bool
} deriving (Show, Generic)
deriveJSON defaultOptions ''ProductPatch

type ImmortelleCmsAPI =
       "product" :> Capture "id" ProductId :> TokenHeader' '["product-read"] :> Get '[JSON] Product
  :<|> "product" :> ReqBody '[JSON] ProductCreate :> TokenHeader' '["product-edit"] :> Post '[JSON] ProductId
  :<|> "product" :> Capture "id" ProductId :> ReqBody '[JSON] ProductPatch :> TokenHeader' '["product-edit"] :> Put '[JSON] ()
  :<|> "product" :> Capture "id" ProductId :> TokenHeader' '["product-edit"] :> Delete '[JSON] ()
  :<|> "product" :> QueryParam "search" Text :> QueryParam "page" PageInfo :> TokenHeader' '["product-read"] :> Get '[JSON] (PagedList Product)
  :<|> AuthSigninMethod
  :<|> AuthSignoutMethod
  :<|> AuthTouchMethod
