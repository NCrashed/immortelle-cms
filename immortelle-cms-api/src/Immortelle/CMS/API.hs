module Immortelle.CMS.API(
    ImmortelleCmsAPI
  , ProductCreate(..)
  , ProductPatch(..)
  ) where

import Data.Set (Set)
import GHC.Generics
import Immortelle.CMS.Aeson
import Immortelle.CMS.Types
import Servant.API

data ProductCreate = ProductCreate {
  cproductCategory      :: ProductCategory
, cproductPatination    :: Maybe Patination
, cproductAuthors       :: Set AuthorCode
, cproductIncrustations :: Set Incrustation
} deriving (Generic)
deriveJSON defaultOptions ''ProductCreate

data ProductPatch = ProductPatch {
  pproductCategory      :: ProductCategory
, pproductPatination    :: Maybe Patination
, pproductAuthors       :: Set AuthorCode
, pproductIncrustations :: Set Incrustation
} deriving (Generic)
deriveJSON defaultOptions ''ProductPatch

type ImmortelleCmsAPI =
       "product" :> Capture "id" ProductId :> Get '[JSON] Product
  :<|> "product" :> ReqBody '[JSON] ProductCreate :> Post '[JSON] ProductId
  :<|> "product" :> Capture "id" ProductId :> ReqBody '[JSON] ProductPatch  :> Put '[JSON] ()
  :<|> "product" :> Capture "id" ProductId :> Delete '[JSON] ()
