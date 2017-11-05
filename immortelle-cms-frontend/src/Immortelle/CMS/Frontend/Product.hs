module Immortelle.CMS.Frontend.Product(
    productAddPage
  ) where

import Control.Lens
import Control.Monad (join)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Ord
import Data.Set (Set)
import Data.Text (Text, pack)
import Immortelle.CMS.API
import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Calendar
import Immortelle.CMS.Frontend.Client
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
  prodE <- productCreateForm
  prodIdE <- dangerResult =<< insertProduct prodE
  widgetHold (pure ()) $ ffor prodIdE $ \i -> success $ "Новое изделие добавлено под ID: " <> showt i
  pure never
  where
    success = elClass "div" "alert alert-success" . text

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
doubleField :: forall t m . MonadWidget t m => Text -> Double -> m (Dynamic t Double)
doubleField label val = do
  tinput <- formGroupText label $ def & textInputConfig_initialValue .~ showt val
  let mres = do
        val <- T.strip <$> _textInput_value tinput
        pure $ case T.double val of
          Left er -> Left $ "Число с точкой содержит ошибку! " <> pack er
          Right (v, left) -> if T.null left then Right v
            else Left $ "Лишние символы: " <> left
  widgetHold (pure ()) $ ffor (updated mres) $ \case
    Left er -> danger er
    _ -> pure ()
  holdDyn val $ fforMaybe (updated mres) $ \case
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

-- | Helper to parse optional double from labeled input field
textField :: forall t m . MonadWidget t m => Text -> m (Dynamic t Text)
textField label = fmap T.strip . _textInput_value <$> formGroupText label def

-- | Helper for checkboxes
checkField :: forall t m . MonadWidget t m => Text -> Bool -> m (Dynamic t Bool)
checkField label val = formGroupLabel label $ divClass "checkbox" $ el "label" $ do
  res <- _checkbox_value <$> checkbox val def
  spanClass "checkbox-material" $ spanClass "check" $ pure ()
  pure res

-- | Displays form that allows to form create request for product
productCreateForm :: forall t m . MonadFront t m => m (Event t ProductCreate)
productCreateForm = horizontalForm $ do
   nameD <- _textInput_value <$> formGroupText "Имя изделия" def
   catD <- _dropdown_value <$> formGroupSelect "Категория" PendantLeaf (pure categoryLabels) def
   catDatumD <- fmap join $ widgetHoldDyn $ categoryForm <$> catD
   patinationD <- patinationForm
   authorsD <- authorsForm
   incrsD <- incrustationsForm
   priceD <- fmap PriceRub <$> doubleField "Цена" 0 -- TODO: other currencies
   creationD <- dayCalendarValue <$> dayCalendarField "Дата изготовления" def
   locD <- mtextField "Место"
   bookedD <- mtextField "Бронь"
   groupD <- checkField "Выложен" False
   submitE <- submitButton "Создать"
   let requestD = ProductCreate
        <$> nameD
        <*> catDatumD
        <*> patinationD
        <*> authorsD
        <*> incrsD
        <*> priceD
        <*> creationD
        <*> locD
        <*> bookedD
        <*> groupD
   pure $ tagPromptlyDyn requestD submitE

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

stoneLabels :: Map Stone Text
stoneLabels = [
    (Labrador, "Лабрадор")
  , (Amethyst, "Аметист")
  , (Quartz, "Кварц")
  , (Rauchtopaz, "Раухтопаз")
  , (Aquamarine, "Аквамарин")
  , (Rhinestone, "Горный хрусталь")
  , (Turquoise, "Бирюза")
  , (Peridot, "Оливин")
  ]

-- | Select stone from dropdown
stoneField :: forall t m . MonadWidget t m => m (Dynamic t Stone)
stoneField = _dropdown_value <$> formGroupSelect "Камень" Quartz (pure stoneLabels) def

-- | Allows to input many values via dynamic count of simple fields
manyInputs :: forall t m a . MonadWidget t m => Int -> m (Dynamic t a) -> m (Dynamic t [a])
manyInputs initialN makeField = mdo
  let makeField' k _ _ = row $ do
        a <- md10 makeField
        delE <- md2 $ primaryButton "Удалить"
        pure (a, delE)
      initalMap = M.fromList $ (\i -> (i, ())) <$> [0 .. initialN-1]
  tmap :: Dynamic t (Map Int (Dynamic t a, Event t ())) <- listWithKeyShallowDiff initalMap updE makeField'
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
        clrs <- fmap S.fromList <$> manyInputs 1 colorField
        pure $ Just . PatinationRainbow <$> clrs
      PatAmmonia -> pure . pure . Just $ PatinationAmmonia
      PatAmmoniaBlue -> pure . pure . Just $ PatinationAmmoniaBlue
      PatSulfur -> pure . pure . Just $ PatinationSulfur
      PatGreen -> pure . pure . Just $ PatinationGreen
      PatStainedGlass -> do
        clrs <- fmap S.fromList <$> manyInputs 1 colorField
        pure $ Just . StainedGlassPaint <$> clrs

-- | Names for author tags
authorTags :: Map AuthorCode Text
authorTags = [
    (AuthorOlga, "Шеффер")
  , (AuthorSveta, "Света")
  , (AuthorPolina, "Полина")
  , (AuthorOther, "Другой")
  ]

-- | Part of form that allows to add authors dynamically
authorsForm :: forall t m . MonadWidget t m => m (Dynamic t (Set (AuthorInfo, Double)))
authorsForm = do
  formGroupLabel "Авторы" $ pure ()
  fmap S.fromList <$> manyInputs 1 authorForm
  where
    authorForm :: m (Dynamic t (AuthorInfo, Double))
    authorForm = panel $ do
      authD <- authInfoForm
      percent <- doubleField "Процент" 100
      pure $ (,) <$> authD <*> percent

    authInfoForm :: m (Dynamic t AuthorInfo)
    authInfoForm = do
      tagD <- _dropdown_value <$> formGroupSelect "Автор" AuthorOlga (pure authorTags) def
      fmap join $ widgetHoldDyn $ makeForm <$> tagD
      where
        makeForm tg = case tg of
          AuthorOlga -> pure . pure . KnownAuthor $ AuthorOlga
          AuthorSveta -> pure . pure . KnownAuthor $ AuthorSveta
          AuthorPolina -> pure . pure . KnownAuthor $ AuthorPolina
          AuthorOther -> do
            name <- textField "Имя"
            pure $ UnknownAuthor <$> name

-- | Tags incrustations types
data IncrustationTag =
    IncrGlass
  | IncrStone
  | IncrPearl
  | IncrBone
  | IncrOther
  deriving (Eq, Ord, Show, Read)

incrustationLabels :: Map IncrustationTag Text
incrustationLabels = [
    (IncrGlass, "Стекло")
  , (IncrStone, "Камень")
  , (IncrPearl, "Жемчуг")
  , (IncrBone, "Кость")
  , (IncrOther, "Другое")
  ]

-- | Part of form that allows to add incrustations dynamically
incrustationsForm :: forall t m . MonadWidget t m => m (Dynamic t (Set Incrustation))
incrustationsForm = do
  formGroupLabel "Вставки" $ pure ()
  fmap S.fromList <$> manyInputs 0 incrustationForm
  where
    incrustationForm :: m (Dynamic t Incrustation)
    incrustationForm = panel $ do
      tagD <- _dropdown_value <$> formGroupSelect "Вставка" IncrGlass (pure incrustationLabels) def
      fmap join $ widgetHoldDyn $ makeForm <$> tagD
      where
        makeForm tg = case tg of
          IncrGlass -> do
            clrs <- fmap S.fromList <$> manyInputs 1 colorField
            pure $ IncrustationGlass <$> clrs
          IncrStone -> do
            stns <- fmap S.fromList <$> manyInputs 1 stoneField
            pure $ IncrustationStone <$> stns
          IncrPearl -> pure . pure $ IncrustationPearl
          IncrBone -> pure . pure $ IncrustationBone
          IncrOther -> pure . pure $ IncrustationOther
