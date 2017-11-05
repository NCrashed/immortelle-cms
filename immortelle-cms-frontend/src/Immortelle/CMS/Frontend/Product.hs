module Immortelle.CMS.Frontend.Product(
    productAddPage
  ) where

import Control.Monad (join)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Ord
import Data.Text (Text, pack)
import Immortelle.CMS.API
import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Calendar
import Immortelle.CMS.Frontend.Menu
import Immortelle.CMS.Frontend.Monad
import Immortelle.CMS.Frontend.Utils
import Immortelle.CMS.Types
import Reflex.Dom
import Web.Reflex.Bootstrap

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T

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

-- | Helper to parse optional double from labeled input field
mdoubleField :: forall t m . MonadWidget t m => Text -> m (Dynamic t (Maybe Double))
mdoubleField label = do
  tinput <- formGroupText label def
  let mres = do
        val <- T.strip <$> _textInput_value tinput
        pure $ if T.null val then Right Nothing else case T.double val of
          Left er -> Left $ "Число с точкой содержит ошибку! " <> pack er
          Right (v, left) -> if T.null left then Right . Just $ v
            else Left $ "Лишние символы: " <> left
  widgetHold (pure ()) $ ffor (updated mres) $ \case
    Left er -> danger er
    _ -> pure ()
  holdDyn Nothing $ fforMaybe (updated mres) $ \case
    Right v -> Just v
    _ -> Nothing

-- | Helper to parse optional double from labeled input field
mintField :: forall t m . MonadWidget t m => Text -> m (Dynamic t (Maybe Int))
mintField label = do
  tinput <- formGroupText label def
  let mres = do
        val <- T.strip <$> _textInput_value tinput
        pure $ if T.null val then Right Nothing else case T.decimal val of
          Left er -> Left $ "Целое число содержит ошибку! " <> pack er
          Right (v, left) -> if T.null left then Right . Just $ v
            else Left $ "Лишние символы: " <> left
  widgetHold (pure ()) $ ffor (updated mres) $ \case
    Left er -> danger er
    _ -> pure ()
  holdDyn Nothing $ fforMaybe (updated mres) $ \case
    Right v -> Just v
    _ -> Nothing

-- | Helper to parse optional double from labeled input field
mtextField :: forall t m . MonadWidget t m => Text -> m (Dynamic t (Maybe Text))
mtextField label = do
  tinput <- formGroupText label def
  pure $ do
    val <- T.strip <$> _textInput_value tinput
    pure $ if T.null val then Nothing else Just val

-- | Displays form that allows to form create request for product
productCreateForm :: forall t m . MonadFront t m => m (Event t ProductCreate)
productCreateForm = horizontalForm $ do
   nameD <- _textInput_value <$> formGroupText "Имя изделия" def
   catD <- _dropdown_value <$> formGroupSelect "Категория" PendantLeaf (pure categoryLabels) def
   catDatumD <- fmap join $ widgetHoldDyn $ categoryForm <$> catD
   creationD <- dayCalendarField "Дата изготовления" def
   patinationD <- patinationForm
   submitE <- submitButton "Создать"
   pure never

-- | Given the fixed category display form for setting cattegory specific data
categoryForm :: forall t m . MonadFront t m => ProductCategory -> m (Dynamic t ProductCategoryData)
categoryForm c = case c of
  PendantLeaf -> do
    pendantLeafWidth <- mdoubleField "Ширина"
    pendantLeafHeight <- mdoubleField "Длина"
    pure $ PendantLeafData <$> pendantLeafWidth <*> pendantLeafHeight
  PendantOther -> do
    pendantOtherWidth <- mdoubleField "Ширина"
    pendantOtherHeight <- mdoubleField "Длина"
    pure $ PendantOtherData <$> pendantOtherWidth <*> pendantOtherHeight
  Necklace -> do
    necklaceWidth <- mdoubleField "Ширина"
    necklaceHeight <- mdoubleField "Длина"
    pure $ NecklaceData <$> necklaceWidth <*> necklaceHeight
  Earings -> do
    earingsWidth <- mdoubleField "Ширина"
    earingsHeight <- mdoubleField "Длина"
    pure $ EaringsData <$> earingsWidth <*> earingsHeight
  Bracelet sub -> do
    mins <- mintField "Мин размер"
    maxs <- mintField "Макс размер"
    pure $ BraceletData <$> pure sub <*> mins <*> maxs
  Ring -> do
    ringSize <- mintField "Размер"
    pure $ RingData <$> ringSize
  Hair sub -> do
    earingsWidth <- mdoubleField "Ширина"
    earingsHeight <- mdoubleField "Длина"
    woodType <- case sub of
      HairPinWood -> mtextField "Тип дерева"
      _ -> pure $ pure Nothing
    pure $ HairData <$> pure sub <*> earingsWidth <*> earingsHeight <*> woodType
  Brooch sub -> do
    width <- mdoubleField "Ширина"
    height <- mdoubleField "Длина"
    pure $ BroochData <$> pure sub <*> width <*> height
  Bookmark -> do
    width <- mdoubleField "Ширина"
    height <- mdoubleField "Длина"
    pure $ BookmarkData <$> width <*> height
  Grand -> pure $ pure GrandData

data PatinationTag = NoPatination
  | PatRainbow
  | PatAmmonia
  | PatAmmoniaBlue
  | PatSulfur
  | PatGreen
  | PatStainedGlass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

patinationLabels :: Map PatinationTag Text
patinationLabels = [
    (NoPatination, "Без обработки")
  , (PatRainbow, "Радужная патина")
  , (PatAmmonia, "Аммиак обычный")
  , (PatAmmonia, "Аммиак голубой")
  , (PatSulfur, "Серная патина")
  , (PatGreen, "Зеленая патина")
  , (PatStainedGlass, "Витражная краска")
  ]

colorLabels :: Map Color Text
colorLabels = [
    (Red, "Красный")
  , (Orange, "Оранжевый")
  , (Yellow, "Желтый")
  , (Green, "Зеленый")
  , (LightBlue, "Голубой")
  , (Blue, "Синий")
  , (Magenta, "Фиолетовый")
  ]

-- | Select color from dropdown
colorField :: forall t m . MonadWidget t m => m (Dynamic t Color)
colorField = _dropdown_value <$> formGroupSelect "Цвет" Red (pure colorLabels) def

-- | Allows to input many values via dynamic count of simple fields
manyInputs :: forall t m a . MonadWidget t m => m (Dynamic t a) -> m (Dynamic t [a])
manyInputs makeField = mdo
  let makeField' k _ _ = row $ do
        a <- md10 makeField
        delE <- md2 $ primaryButton "Удалить"
        pure (a, delE)
  tmap :: Dynamic t (Map Int (Dynamic t a, Event t ())) <- listWithKeyShallowDiff [(0, ())] updE makeField'
  let valmap = joinDynThroughMap $ fmap fst <$> tmap :: Dynamic t (Map Int a)
      delmap = fmap snd <$> tmap :: Dynamic t (Map Int (Event t ()))
      delmap' = switchPromptlyDyn $ mergeMap <$> delmap :: Event t (Map Int ())
      delmap'' = fmap (const Nothing) <$> delmap' :: Event t (Map Int (Maybe ()))
      maximum' :: [Int] -> Int
      maximum' [] = 0
      maximum' xs = maximum xs
      lastid = maximum' . M.keys <$> tmap :: Dynamic t Int
  addE <- row $ md1 (pure ()) >> md2 (primaryButton "Больше элементов")
  let addE' = attachPromptlyDynWith (\i _ -> [(i+1, Just ())]) lastid addE :: Event t (Map Int (Maybe ()))
      updE = delmap'' <> addE'
  pure $ fmap snd . sortBy (comparing fst) . M.toList <$> valmap
  where
    md1 = elClass "div" "col-md-1"
    md10 = elClass "div" "col-md-10"

-- | Part of form that allows dynamically input patination types with colors
patinationForm :: MonadWidget t m => m (Dynamic t (Maybe Patination))
patinationForm = do
  tagD <- _dropdown_value <$> formGroupSelect "Тип патины" NoPatination (pure patinationLabels) def
  fmap join $ widgetHoldDyn $ makeForm <$> tagD
  where
    makeForm tg = case tg of
      NoPatination -> pure $ pure Nothing
      PatRainbow -> do
        clrs <- fmap S.fromList <$> manyInputs colorField
        pure $ Just . PatinationRainbow <$> clrs
      PatAmmonia -> pure . pure . Just $ PatinationAmmonia
      PatAmmoniaBlue -> pure . pure . Just $ PatinationAmmoniaBlue
      PatSulfur -> pure . pure . Just $ PatinationSulfur
      PatGreen -> pure . pure . Just $ PatinationGreen
      PatStainedGlass -> do
        clrs <- fmap S.fromList <$> manyInputs colorField
        pure $ Just . StainedGlassPaint <$> clrs
