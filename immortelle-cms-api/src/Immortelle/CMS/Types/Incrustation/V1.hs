module Immortelle.CMS.Types.Incrustation.V1(
    Incrustation(..)
  , displayIncrustation
  ) where

import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson
import Immortelle.CMS.Types.Color
import Immortelle.CMS.Types.Stone

import qualified Data.Text as T
import qualified Data.Set as S

-- | Insertions
data Incrustation =
    IncrustationGlass (Set Color)
  | IncrustationStone (Set Stone)
  | IncrustationPearl
  | IncrustationBone
  | IncrustationOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Incrustation
deriveJSON defaultOptions ''Incrustation

displayIncrustation :: Incrustation -> Text
displayIncrustation v = case v of
  IncrustationGlass clrs -> "Стекло " <> T.unwords (fmap displayColor . S.toList $ clrs)
  IncrustationStone stns -> "Камень " <> T.unwords (fmap displayStone . S.toList $ stns)
  IncrustationPearl -> "Жемчуг"
  IncrustationBone -> "Кость"
  _ -> ""
