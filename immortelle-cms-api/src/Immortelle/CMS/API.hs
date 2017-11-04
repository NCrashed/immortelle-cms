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
  deriving (Eq, Ord, Generic)
deriveJSON defaultOptions ''AuthorInfo

data ProductCreate = ProductCreate {
  cproductName          :: Text
, cproductCategory      :: ProductCategoryData
, cproductPatination    :: Maybe Patination
, cproductAuthors       :: Set (AuthorInfo, Double)
, cproductIncrustations :: Set Incrustation
, cproductPrice         :: Price
, cproductCreation      :: Maybe Day
, cproductLocation      :: Maybe Text
, cproductBooked        :: Maybe Text
, cproductInGroup       :: Bool
} deriving (Generic)
deriveJSON defaultOptions ''ProductCreate

data ProductPatch = ProductPatch {
  pproductName          :: Text
, pproductCategory      :: ProductCategoryData
, pproductPatination    :: Maybe Patination
, pproductAuthors       :: Set (AuthorInfo, Double)
, pproductIncrustations :: Set Incrustation
, pproductPrice         :: Price
, pproductCreation      :: Maybe Day
, pproductLocation      :: Maybe Text
, pproductBooked        :: Maybe Text
, pproductInGroup       :: Bool
} deriving (Generic)
deriveJSON defaultOptions ''ProductPatch

type ImmortelleCmsAPI =
       "product" :> Capture "id" ProductId :> TokenHeader' '["product-read"] :> Get '[JSON] Product
  :<|> "product" :> ReqBody '[JSON] ProductCreate :> TokenHeader' '["product-edit"] :> Post '[JSON] ProductId
  :<|> "product" :> Capture "id" ProductId :> ReqBody '[JSON] ProductPatch :> TokenHeader' '["product-edit"] :> Put '[JSON] ()
  :<|> "product" :> Capture "id" ProductId :> TokenHeader' '["product-edit"] :> Delete '[JSON] ()
  :<|> AuthSigninMethod
  :<|> AuthSignoutMethod
  :<|> AuthTouchMethod
