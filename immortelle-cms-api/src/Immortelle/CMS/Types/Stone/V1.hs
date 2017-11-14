module Immortelle.CMS.Types.Stone.V1(
    Stone(..)
  , displayStone
  ) where

import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

-- | Kinds of stones
data Stone =
    Labrador
  | Amethyst
  | Quartz
  | Rauchtopaz
  | Aquamarine
  | Rhinestone
  | Turquoise
  | Peridot
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Stone
deriveJSON defaultOptions ''Stone

displayStone :: Stone -> Text
displayStone st = case st of
  Labrador -> "Лабрадор"
  Amethyst -> "Аметист"
  Quartz -> "Кварц"
  Rauchtopaz -> "Раухтопаз"
  Aquamarine -> "Аквамарин"
  Rhinestone -> "Горный хрусталь"
  Turquoise -> "Бирюза"
  Peridot -> "Оливин"
