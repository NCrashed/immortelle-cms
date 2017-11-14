module Immortelle.CMS.Types.Product.V1(
    ProductId(..)
  , Product(..)
  , VendorCode(..)
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

-- | Unique id of product
newtype ProductId = ProductId { unProductId :: Word }
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''ProductId
deriveJSON defaultNewtypeOptions ''ProductId

instance FromHttpApiData ProductId where
  parseUrlPiece = fmap ProductId . parseUrlPiece

instance ToHttpApiData ProductId where
  toUrlPiece = toUrlPiece . unProductId

-- | User friendly display of product id and properties (артикул)
data VendorCode = VendorCode {
  vcodeId            :: ProductId
, vcodeCategory      :: ProductCategory
, vcodePatination    :: Maybe Patination
, vcodeAuthors       :: Set AuthorCode
, vcodeIncrustations :: Set Incrustation
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''VendorCode
deriveJSON defaultOptions ''VendorCode

data Product = Product {
  productId            :: ProductId
, productName          :: Text
, productCategory      :: ProductCategoryData
, productPatination    :: Maybe Patination
, productAuthors       :: Set (Author, Double)
, productIncrustations :: Set Incrustation
, productPrice         :: Price
, productCreation      :: Maybe Day
, productLocation      :: Maybe Text
, productBooked        :: Maybe Text
, productInGroup       :: Bool
, productTimestamp     :: UTCTime
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Product
deriveJSON defaultOptions ''Product

-- | Extract vendor code from product info
productVendorCode :: Product -> VendorCode
productVendorCode Product{..} = VendorCode {
    vcodeId = productId
  , vcodeCategory = productCategoryFromData productCategory
  , vcodePatination = productPatination
  , vcodeAuthors = S.map (authorCode . fst) productAuthors
  , vcodeIncrustations = productIncrustations
  }
