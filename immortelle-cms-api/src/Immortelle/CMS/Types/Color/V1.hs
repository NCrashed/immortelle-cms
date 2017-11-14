module Immortelle.CMS.Types.Color.V1(
    Color(..)
  , displayColor
  ) where

import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

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

displayColor :: Color -> Text
displayColor c = case c of
  Red -> "Красный"
  Orange -> "Оранжевый"
  Yellow -> "Желтый"
  Green -> "Зеленый"
  LightBlue -> "Голубой"
  Blue -> "Синий"
  Magenta -> "Фиолетовый"
