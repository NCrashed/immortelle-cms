module Immortelle.CMS.Types.Color.V2(
    Color(..)
  , displayColor
  ) where

import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

import qualified Immortelle.CMS.Types.Color.V1 as V1

-- TODO: add more
data Color =
    Red
  | Orange
  | Yellow
  | Green
  | LightBlue
  | Blue
  | Magenta
  | White
  | Black
  | Pink
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 1 'extension ''Color
deriveJSON defaultOptions ''Color

instance Migrate Color where
  type MigrateFrom Color = V1.Color
  migrate v = case v of
    V1.Red -> Red
    V1.Orange -> Orange
    V1.Yellow -> Yellow
    V1.Green -> Green
    V1.LightBlue -> LightBlue
    V1.Blue -> Blue
    V1.Magenta -> Magenta
  {-# INLINE migrate #-}

displayColor :: Color -> Text
displayColor c = case c of
  Red -> "Красный"
  Orange -> "Оранжевый"
  Yellow -> "Желтый"
  Green -> "Зеленый"
  LightBlue -> "Голубой"
  Blue -> "Синий"
  Magenta -> "Фиолетовый"
  White -> "Белый"
  Black -> "Чёрный"
  Pink -> "Розовый"
