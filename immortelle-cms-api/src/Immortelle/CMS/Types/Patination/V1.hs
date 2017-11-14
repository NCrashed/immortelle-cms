module Immortelle.CMS.Types.Patination.V1(
    Patination(..)
  , displayPatination
  ) where

import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

import Immortelle.CMS.Types.Color

import qualified Data.Set as S
import qualified Data.Text as T

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

displayPatination :: Patination -> Text
displayPatination p = case p of
  PatinationRainbow clrs -> "Радужная " <> T.unwords (fmap displayColor . S.toList $ clrs)
  PatinationAmmonia -> "Аммиак"
  PatinationAmmoniaBlue -> "Аммиак синий"
  PatinationSulfur -> "Сера"
  PatinationGreen -> "Зеленая"
  StainedGlassPaint clrs -> "Витражная краска " <> T.unwords (fmap displayColor . S.toList $ clrs)
