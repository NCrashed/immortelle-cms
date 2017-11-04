module Immortelle.CMS.Frontend.Product(
    productAddPage
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Immortelle.CMS.API
import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Menu
import Immortelle.CMS.Frontend.Monad
import Immortelle.CMS.Types
import Reflex.Dom
import Web.Reflex.Bootstrap

productAddPage :: forall t m . MonadFront t m => m (Event t CmsMenuItem)
productAddPage = do
  _ <- productCreateForm
  pure never

categoryLabels :: Map ProductCategory Text
categoryLabels = [
    (PendantLeaf, "Кулон Лист")
  , (PendantOther, "Кулон Другой")
  , (Necklace, "Колье")
  , (Earings, "Серьги")
  , (Bracelet BraceletNet, "Браслет Сетка")
  , (Bracelet BraceletLace, "Браслет Кружево")
  , (Bracelet BraceletLeaf, "Браслет Лист")
  , (Ring, "Кольцо")
  , (Hair HairPinWood, "Шпилька Дерево")
  , (Hair HairPinCopper, "Шпилька Медь")
  , (Hair Crest, "Гребень")
  , (Hair Barrette, "Заколка")
  , (Brooch BroochUsual, "Брошь Обычная")
  , (Brooch HatPin, "Шляпная Булавка")
  , (Brooch Fibula, "Фибула")
  , (Bookmark, "Закладка")
  , (Grand, "Гранд") ]

-- | Displays form that allows to form create request for product
productCreateForm :: forall t m . MonadFront t m => m (Event t ProductCreate)
productCreateForm = horizontalForm $ do
   nameD <- _textInput_value <$> formGroupText "Имя изделия" def
   catD <- _dropdown_value <$> formGroupSelect "Категория" PendantLeaf (pure categoryLabels) def
   pure never
