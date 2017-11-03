module Immortelle.CMS.Types(
    BraceletType(..)
  , HairType(..)
  , BroochType(..)
  , ProductCategory(..)
  , AuthorCode(..)
  , Author(..)
  , Color(..)
  , Patination(..)
  , Stone(..)
  , Incrustation(..)
  , VendorCode(..)
  ) where

import Data.Acid
import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics

import qualified Data.Set as S

-- | Kinds of bracelets
data BraceletType = BraceletNet | BraceletLace | BraceletLeaf
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''BraceletType

-- | Kind of product that is weared in hair
data HairType = HairPinWood | HairPinCopper | Crest | Barrette
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''HairType

-- | Kinds of brooch
data BroochType = BroochUsual | HatPin | Fibula
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''BroochType

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

-- | Codes of authors in vendoc code
data AuthorCode =
    AuthorOlga
  | AuthorSveta
  | AuthorPolina
  | AuthorOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''AuthorCode

-- | Author of product
data Author = Author {
  authorName :: Text
, authorCode :: AuthorCode
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Author

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

-- | Kinds of stones
data Stone =
    Labrador
  | Amethyst
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Stone

-- | Insertions
data Incrustation =
    IncrustationGlass (Set Color)
  | IncrustationStore (Set Stone)
  | IncrustationPearl
  | IncrustationOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Incrustation

-- | User friendly display of product id and properties (артикул)
data VendorCode = VendorCode {
  vcodeId            :: Word
, vcodeCategory      :: ProductCategory
, vcodePatination    :: Maybe Patination
, vcodeAuthors       :: Set AuthorCode
, vcodeIncrustations :: Set Incrustation
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''VendorCode

data Product = Product {
  productId            :: Word
, productCategory      :: ProductCategory
, productPatination    :: Maybe Patination
, productAuthors       :: Set Author
, productIncrustations :: Set Incrustation
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Product

productVendorCode :: Product -> VendorCode
productVendorCode Product{..} = VendorCode {
    vcodeId = productId
  , vcodeCategory = productCategory
  , vcodePatination = productPatination
  , vcodeAuthors = S.map authorCode productAuthors
  , vcodeIncrustations = productIncrustations
  }
