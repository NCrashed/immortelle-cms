module Immortelle.CMS.Types.Incrustation.V2(
    Incrustation(..)
  , PearlType(..)
  , displayIncrustation
  , IncrustationData(..)
  , displayIncrustationData
  , incrustationFromData
  , incrustationToData
  ) where

import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson
import Immortelle.CMS.Types.Color
import Immortelle.CMS.Types.Stone

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Immortelle.CMS.Types.Incrustation.V1 as V1

-- | Insertions
data Incrustation =
    IncrustationGlass (Set Color)
  | IncrustationGlassDichroic (Set Color)
  | IncrustationStone (Set Stone)
  | IncrustationPearl
  | IncrustationPorcelain (Set Color)
  | IncrustationBone
  | IncrustationOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 1 'extension ''Incrustation
deriveJSON defaultOptions ''Incrustation

instance Migrate Incrustation where
  type MigrateFrom Incrustation = V1.Incrustation
  migrate v = case v of
    V1.IncrustationGlass vs -> IncrustationGlass vs
    V1.IncrustationStone vs -> IncrustationStone vs
    V1.IncrustationPearl -> IncrustationPearl
    V1.IncrustationBone -> IncrustationBone
    V1.IncrustationOther -> IncrustationOther

displayIncrustation :: Incrustation -> Text
displayIncrustation v = case v of
  IncrustationGlass clrs -> "Стекло " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationGlassDichroic clrs -> "Стекло дихроическое " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationStone stns -> "Камень " <> T.unwords (fmap displayStone . S.toList $ stns)
  IncrustationPearl -> "Жемчуг"
  IncrustationPorcelain clrs -> "Фарфор " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationBone -> "Кость"
  _ -> ""

-- | Kinds of pearls
data PearlType = PearlDark | PearlBright
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''PearlType
deriveJSON defaultOptions ''PearlType

-- | Insertions specific data that doesn't go to vendor code
data IncrustationData =
    IncrustationDataGlass (Set Color)
  | IncrustationDataGlassDichroic (Set Color)
  | IncrustationDataStone (Set Stone)
  | IncrustationDataPearl PearlType
  | IncrustationDataPorcelain (Set Color)
  | IncrustationDataBone
  | IncrustationDataOther Text
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''IncrustationData
deriveJSON defaultOptions ''IncrustationData

-- | Display data of incrustation as human readable and searchable string
displayIncrustationData :: IncrustationData -> Text
displayIncrustationData v = case v of
  IncrustationDataGlass clrs -> "Стекло " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationDataGlassDichroic clrs -> "Стекло дихроическое " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationDataStone stns -> "Камень " <> T.unwords (fmap displayStone . S.toList $ stns)
  IncrustationDataPearl ct -> "Жемчуг " <> case ct of
    PearlDark -> " тёмный"
    PearlBright -> " светлый"
  IncrustationDataPorcelain clrs -> "Фарфор " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationDataBone -> "Кость"
  IncrustationDataOther other -> other

-- | Extract least specific tag from incrustation data record
incrustationFromData :: IncrustationData -> Incrustation
incrustationFromData v = case v of
  IncrustationDataGlass cls -> IncrustationGlass cls
  IncrustationDataGlassDichroic cls -> IncrustationGlassDichroic cls
  IncrustationDataStone sts -> IncrustationStone sts
  IncrustationDataPearl _ -> IncrustationPearl
  IncrustationDataPorcelain cls -> IncrustationPorcelain cls
  IncrustationDataBone -> IncrustationBone
  IncrustationDataOther _ -> IncrustationOther

-- | Extend specific tag to incrustation data record
incrustationToData :: Incrustation -> IncrustationData
incrustationToData v = case v of
  IncrustationGlass cls -> IncrustationDataGlass cls
  IncrustationGlassDichroic cls -> IncrustationDataGlassDichroic cls
  IncrustationStone sts -> IncrustationDataStone sts
  IncrustationPearl -> IncrustationDataPearl PearlBright
  IncrustationPorcelain cls -> IncrustationDataPorcelain cls
  IncrustationBone -> IncrustationDataBone
  IncrustationOther -> IncrustationDataOther ""
