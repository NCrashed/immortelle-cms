module Immortelle.CMS.Frontend.Menu(
    CmsMenu
  , CmsMenuItem(..)
  , MenuWidget(..)
  , menuWidget
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text
import Reflex.Dom

import qualified Data.Map.Strict as M

import Web.Reflex.Bootstrap
import Immortelle.CMS.Frontend.Utils

-- | Tag to track particular menu type
data CmsMenu

-- | Availiable items in menu
data CmsMenuItem =
    ProductAddPage
  | ProductListPage
  deriving (Eq, Ord, Show)

instance MenuWidget CmsMenu where
  type MenuItem CmsMenu = CmsMenuItem
  menuBrand _ = "Immortelle"
  menuItemLabel _ v = case v of
    ProductAddPage -> "Добавить изделие"
    ProductListPage -> "Просмотр изделий"
  menuLogoutLabel _ = "Выйти"
