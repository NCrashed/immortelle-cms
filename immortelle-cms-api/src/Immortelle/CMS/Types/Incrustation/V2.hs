module Immortelle.CMS.Types.Incrustation.V2(
    V1.Incrustation(..)
  , V1.displayIncrustation
  , IncrustationData(..)
  ) where

import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson
import Immortelle.CMS.Types.Color
import Immortelle.CMS.Types.Stone

import qualified Immortelle.CMS.Types.Incrustation.V1 as V1

-- | Insertions specific data that doesn't go to vendor code
data IncrustationData =
    IncrustationDataGlass
  | IncrustationDataStone
  | IncrustationDataPearl
  | IncrustationDataBone
  | IncrustationDataOther Text
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''IncrustationData
deriveJSON defaultOptions ''IncrustationData
