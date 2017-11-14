module Immortelle.CMS.Types.Category.V2(
    V1.BraceletType(..)
  , V1.HairType(..)
  , V1.BroochType(..)
  , V1.ProductCategory(..)
  , ProductCategoryData(..)
  , productCategoryFromData
  , V1.displayCategory
  ) where

import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

import qualified Immortelle.CMS.Types.Category.V1 as V1

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
      braceletSubType :: V1.BraceletType
    , braceletSizeMin :: Maybe Int
    , braceletSizeMax :: Maybe Int
    }
  | RingData {
      ringSize       :: Maybe Double
    , ringAdjustable :: Bool
    }
  | HairData {
      hairSubType  :: V1.HairType
    , hairWidth    :: Maybe Double
    , hairHeight   :: Maybe Double
    , hairWoodType :: Maybe Text
    }
  | BroochData {
      broochSubType :: V1.BroochType
    , broochWidth   :: Maybe Double
    , broochHeight  :: Maybe Double
    }
  | BookmarkData {
      bookmarkWidth   :: Maybe Double
    , bookmarkHeight  :: Maybe Double
    }
  | GrandData
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 1 'extension ''ProductCategoryData
deriveJSON defaultOptions ''ProductCategoryData

instance Migrate ProductCategoryData where
  type MigrateFrom ProductCategoryData = V1.ProductCategoryData
  migrate v = case v of
    V1.PendantLeafData{..} -> PendantLeafData{..}
    V1.PendantOtherData{..} -> PendantOtherData{..}
    V1.NecklaceData{..} -> NecklaceData{..}
    V1.EaringsData{..} -> EaringsData{..}
    V1.BraceletData{..} -> BraceletData{..}
    V1.RingData{..} -> RingData {
        ringSize = fromIntegral <$> ringSize
      , ringAdjustable = False 
      }
    V1.HairData{..} -> HairData{..}
    V1.BroochData{..} -> BroochData{..}
    V1.BookmarkData{..} -> BookmarkData{..}
    V1.GrandData -> GrandData

-- | Extract category tag from extenden data
productCategoryFromData :: ProductCategoryData -> V1.ProductCategory
productCategoryFromData v = case v of
  PendantLeafData{..} -> V1.PendantLeaf
  PendantOtherData{..} -> V1.PendantOther
  NecklaceData{..} -> V1.Necklace
  EaringsData{..} -> V1.Earings
  BraceletData{..} -> V1.Bracelet braceletSubType
  RingData{..} -> V1.Ring
  HairData{..} -> V1.Hair hairSubType
  BroochData{..} -> V1.Brooch broochSubType
  BookmarkData{..} -> V1.Bookmark
  GrandData  -> V1.Grand
