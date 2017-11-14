module Immortelle.CMS.Types.Price.V1(
    Price(..)
  , displayPrice
  ) where

import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson
import Text.Printf

import qualified Data.Text as T

-- | Price in different currencies
data Price = PriceRub Double
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Price
deriveJSON defaultOptions ''Price

displayPrice :: Price -> Text
displayPrice p = case p of
  PriceRub v -> T.pack . printf "%.2f" $ v
