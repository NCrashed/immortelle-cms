module Immortelle.CMS.Types(
    BraceletType(..)
  , HairType(..)
  , BroochType(..)
  , ProductCategory(..)
  , ProductCategoryData(..)
  , productCategoryFromData
  , AuthorCode(..)
  , Author(..)
  , Color(..)
  , Patination(..)
  , Stone(..)
  , Incrustation(..)
  , ProductId(..)
  , VendorCode(..)
  , Price(..)
  , Product(..)
  ) where

import Data.Map.Strict (Map)
import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Immortelle.CMS.Aeson
import Web.HttpApiData

import qualified Data.Set as S

-- | Kinds of bracelets
data BraceletType = BraceletNet | BraceletLace | BraceletLeaf
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''BraceletType
deriveJSON defaultOptions ''BraceletType

-- | Kind of product that is weared in hair
data HairType = HairPinWood | HairPinCopper | Crest | Barrette
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''HairType
deriveJSON defaultOptions ''HairType

-- | Kinds of brooch
data BroochType = BroochUsual | HatPin | Fibula
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''BroochType
deriveJSON defaultOptions ''BroochType

-- | Category of product
data ProductCategory =
    PendantLeaf
  | PendantOther
  | Necklace
  | Earings
  | Bracelet BraceletType
  | Ring
  | Hair HairType
  | Brooch BroochType
  | Bookmark
  | Grand
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''ProductCategory
deriveJSON defaultOptions ''ProductCategory

-- | Category with data
data ProductCategoryData =
    PendantLeafData {
      pendantLeafWidth  :: Maybe Double
    , pendantLeafHeight :: Maybe Double
    }
  | PendantOtherData {
      pendantOtherWidth  :: Maybe Double
    , pendantOtherHeight :: Maybe Double
    }
  | NecklaceData {
      necklaceWidth  :: Maybe Double
    , necklaceHeight :: Maybe Double
    }
  | EaringsData {
      earingsWidth  :: Maybe Double
    , earingsHeight :: Maybe Double
    }
  | BraceletData {
      braceletSubType :: BraceletType
    , braceletSize :: Maybe (Int, Int)
    }
  | RingData {
      ringSize :: Maybe Int
    }
  | HairData {
      hairSubType  :: HairType
    , hairWidth    :: Maybe Double
    , hairHeight   :: Maybe Double
    , hairWoodType :: Maybe Text
    }
  | BroochData {
      broochSubType :: BroochType
    , broochWidth   :: Maybe Double
    , broochHeight  :: Maybe Double
    }
  | BookmarkData {
      bookmarkWidth   :: Maybe Double
    , bookmarkHeight  :: Maybe Double
    }
  | GrandData
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''ProductCategoryData
deriveJSON defaultOptions ''ProductCategoryData

-- | Extract category tag from extenden data
productCategoryFromData :: ProductCategoryData -> ProductCategory
productCategoryFromData v = case v of
  PendantLeafData{..} -> PendantLeaf
  PendantOtherData{..} -> PendantOther
  NecklaceData{..} -> Necklace
  EaringsData{..} -> Earings
  BraceletData{..} -> Bracelet braceletSubType
  RingData{..} -> Ring
  HairData{..} -> Hair hairSubType
  BroochData{..} -> Brooch broochSubType
  BookmarkData{..} -> Bookmark
  GrandData  -> Grand

-- | Codes of authors in vendoc code
data AuthorCode =
    AuthorOlga
  | AuthorSveta
  | AuthorPolina
  | AuthorOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''AuthorCode
deriveJSON defaultOptions ''AuthorCode

-- | Author of product
data Author = Author {
  authorName :: Text
, authorCode :: AuthorCode
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Author
deriveJSON defaultOptions ''Author

-- TODO: add more
data Color =
    Red
  | Orange
  | Yellow
  | Green
  | LightBlue
  | Blue
  | Magenta
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Color
deriveJSON defaultOptions ''Color

-- | Kinds of patination post process
data Patination =
    PatinationRainbow (Set Color)
  | PatinationAmmonia
  | PatinationAmmoniaBlue
  | PatinationSulfur
  | PatinationGreen
  | StainedGlassPaint (Set Color)
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Patination
deriveJSON defaultOptions ''Patination

-- | Kinds of stones
data Stone =
    Labrador
  | Amethyst
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Stone
deriveJSON defaultOptions ''Stone

-- | Insertions
data Incrustation =
    IncrustationGlass (Set Color)
  | IncrustationStore (Set Stone)
  | IncrustationPearl
  | IncrustationOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Incrustation
deriveJSON defaultOptions ''Incrustation

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

-- | Price in different currencies
data Price = PriceRub Double
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Price
deriveJSON defaultOptions ''Price

data Product = Product {
  productId            :: ProductId
, productName          :: Text
, productCategory      :: ProductCategoryData
, productPatination    :: Maybe Patination
, productAuthors       :: Set (Author, Double)
, productIncrustations :: Set Incrustation
, productPrice         :: Price
, productTimestamp     :: UTCTime
, productLocation      :: Maybe Text
, productBooked        :: Maybe Text
, productInGroup       :: Bool
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Product
deriveJSON defaultOptions ''Product

productVendorCode :: Product -> VendorCode
productVendorCode Product{..} = VendorCode {
    vcodeId = productId
  , vcodeCategory = productCategoryFromData productCategory
  , vcodePatination = productPatination
  , vcodeAuthors = S.map (authorCode . fst) productAuthors
  , vcodeIncrustations = productIncrustations
  }
