module Immortelle.CMS.Types.Product.V2(
    V1.ProductId(..)
  , Product(..)
  , V1.VendorCode(..)
  , productVendorCode
  ) where

import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime, Day)
import GHC.Generics
import Immortelle.CMS.Aeson
import Web.HttpApiData

import Immortelle.CMS.Types.Author
import Immortelle.CMS.Types.Category
import Immortelle.CMS.Types.Incrustation
import Immortelle.CMS.Types.Patination
import Immortelle.CMS.Types.Price

import qualified Data.Set as S

import qualified Immortelle.CMS.Types.Product.V1 as V1

data Product = Product {
  productId            :: V1.ProductId
, productName          :: Text
, productCategory      :: ProductCategoryData
, productPatination    :: Maybe Patination
, productAuthors       :: Set (Author, Double)
, productIncrustations :: Set IncrustationData
, productPrice         :: Price
, productCreation      :: Maybe Day
, productLocation      :: Maybe Text
, productBooked        :: Maybe Text
, productInGroup       :: Bool
, productTimestamp     :: UTCTime
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 1 'extension ''Product
deriveJSON defaultOptions ''Product

instance Migrate Product where
  type MigrateFrom Product = V1.Product
  migrate V1.Product{..} = Product {
      productIncrustations = S.map incrustationToData productIncrustations
    , ..
    }

-- | Extract vendor code from product info
productVendorCode :: Product -> V1.VendorCode
productVendorCode Product{..} = V1.VendorCode {
    V1.vcodeId = productId
  , V1.vcodeCategory = productCategoryFromData productCategory
  , V1.vcodePatination = productPatination
  , V1.vcodeAuthors = S.map (authorCode . fst) productAuthors
  , V1.vcodeIncrustations = S.map incrustationFromData productIncrustations
  }
