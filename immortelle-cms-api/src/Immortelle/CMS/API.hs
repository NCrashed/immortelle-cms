module Immortelle.CMS.API(
    ImmortelleCmsAPI
  , AuthorInfo(..)
  , ProductCreate(..)
  , ProductPatch(..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson
import Immortelle.CMS.Types
import Servant.API

data AuthorInfo = KnownAuthor AuthorCode | UnknownAuthor Text
  deriving (Eq, Ord, Generic)
deriveJSON defaultOptions ''AuthorInfo

data ProductCreate = ProductCreate {
  cproductCategory      :: ProductCategory
, cproductPatination    :: Maybe Patination
, cproductAuthors       :: Set AuthorInfo
, cproductIncrustations :: Set Incrustation
} deriving (Generic)
deriveJSON defaultOptions ''ProductCreate

data ProductPatch = ProductPatch {
  pproductCategory      :: ProductCategory
, pproductPatination    :: Maybe Patination
, pproductAuthors       :: Set AuthorInfo
, pproductIncrustations :: Set Incrustation
} deriving (Generic)
deriveJSON defaultOptions ''ProductPatch

type ImmortelleCmsAPI =
       "product" :> Capture "id" ProductId :> Get '[JSON] Product
  :<|> "product" :> ReqBody '[JSON] ProductCreate :> Post '[JSON] ProductId
  :<|> "product" :> Capture "id" ProductId :> ReqBody '[JSON] ProductPatch  :> Put '[JSON] ()
  :<|> "product" :> Capture "id" ProductId :> Delete '[JSON] ()
