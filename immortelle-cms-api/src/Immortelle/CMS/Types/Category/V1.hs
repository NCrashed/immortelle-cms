module Immortelle.CMS.Types.Category.V1(
    BraceletType(..)
  , HairType(..)
  , BroochType(..)
  , ProductCategory(..)
  , ProductCategoryData(..)
  , productCategoryFromData
  , displayCategory
  ) where

import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

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
    , braceletSizeMin :: Maybe Int
    , braceletSizeMax :: Maybe Int
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

displayCategory :: ProductCategory -> Text
displayCategory c = case c of
  PendantLeaf -> "Кулон лист"
  PendantOther -> "Кулон"
  Necklace -> "Ожерелье"
  Earings -> "Серьги"
  Bracelet bt -> "Браслет " <> case bt of
    BraceletNet -> "сетка"
    BraceletLace -> "кружево"
    BraceletLeaf -> "лист"
  Ring -> "Кольцо"
  Hair ht -> "Волосы " <> case ht of
    HairPinWood -> "шпилька дерево"
    HairPinCopper -> "шпилька медь"
    Crest -> "гребень"
    Barrette -> "заколка"
  Brooch bt -> "Брошка " <> case bt of
    BroochUsual -> ""
    HatPin -> "шляпная булавка"
    Fibula -> "фибула"
  Bookmark -> "Закладка"
  Grand -> "Гранд"
