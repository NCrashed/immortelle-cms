module Immortelle.CMS.Frontend.Product(
    productAddPage
  ) where

import Control.Lens
import Control.Monad (join)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe
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
  _ <- productCreate
  pure never

productCreate :: forall t m . MonadFront t m => m (Event t ProductId )
productCreate = mdo
  res :: Dynamic t (Event t ProductId, Dynamic t ProductCreate) <- widgetHold (body defaultProductCreate) $ body . resetProductCreate <$> reqE
  let reqD :: Dynamic t ProductCreate = join $ snd <$> res
      succE :: Event t ProductId = switchPromptlyDyn $ fst <$> res
      reqE :: Event t ProductCreate = tagPromptlyDyn reqD succE
  -- widgetHold (pure ()) $ ffor (updated reqD) $ \val -> success $ showt val
  widgetHold (pure ()) $ ffor succE $ \i -> success $ "Новое изделие добавлено под ID: " <> showt i
  pure succE
  where
    body v = do
      prodDyn <- holdDyn v =<< productCreateForm v
      prodIdE <- dangerResult =<< insertProduct (updated prodDyn)
      pure (prodIdE, prodDyn)
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
mdoubleField :: forall t m . MonadWidget t m => Text -> Maybe Double -> m (Dynamic t (Maybe Double))
mdoubleField label mt = do
  let initText = maybe "" showt mt
  tinput <- formGroupText label $ def & textInputConfig_initialValue .~ initText
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
mintField :: forall t m . MonadWidget t m => Text -> Maybe Int -> m (Dynamic t (Maybe Int))
mintField label val0 = do
  let initText = maybe "" showt val0
  tinput <- formGroupText label $ def & textInputConfig_initialValue .~ initText
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
mtextField :: forall t m . MonadWidget t m => Text -> Maybe Text -> m (Dynamic t (Maybe Text))
mtextField label val0 = do
  let initText = fromMaybe "" val0
  tinput <- formGroupText label $ def & textInputConfig_initialValue .~ initText
  pure $ do
    val <- T.strip <$> _textInput_value tinput
    pure $ if T.null val then Nothing else Just val

-- | Helper to parse optional double from labeled input field
textField :: forall t m . MonadWidget t m => Text -> Text -> m (Dynamic t Text)
textField label initText = fmap T.strip . _textInput_value <$> formGroupText label (def & textInputConfig_initialValue .~ initText)

-- | Helper for checkboxes
checkField :: forall t m . MonadWidget t m => Text -> Bool -> m (Dynamic t Bool)
checkField label val = formGroupLabel label $ divClass "checkbox" $ el "label" $ do
  res <- _checkbox_value <$> checkbox val def
  spanClass "checkbox-material" $ spanClass "check" $ pure ()
  pure res

-- | Create request with no data
defaultProductCreate :: ProductCreate
defaultProductCreate = ProductCreate {
    cproductName          = ""
  , cproductCategory      = PendantLeafData Nothing Nothing
  , cproductPatination    = Nothing
  , cproductAuthors       = []
  , cproductIncrustations = []
  , cproductPrice         = PriceRub 0
  , cproductCreation      = Nothing
  , cproductLocation      = Nothing
  , cproductBooked        = Nothing
  , cproductInGroup       = False
  }

resetProductCreate :: ProductCreate -> ProductCreate
resetProductCreate p = p {
    cproductName          = ""
  , cproductPatination    = Nothing
  , cproductIncrustations = []
  , cproductPrice         = PriceRub 0
  , cproductCreation      = Nothing
  , cproductLocation      = Nothing
  , cproductBooked        = Nothing
  , cproductInGroup       = False
  }
-- | Displays form that allows to form create request for product
productCreateForm :: forall t m . MonadFront t m => ProductCreate -> m (Event t ProductCreate)
productCreateForm pc0 = horizontalForm $ do
   nameD <- _textInput_value <$> formGroupText "Имя изделия" (def & textInputConfig_initialValue .~ cproductName pc0)
   catD <- _dropdown_value <$> formGroupSelect "Категория" (productCategoryFromData $ cproductCategory pc0) (pure categoryLabels) def
   catDatumD <- fmap join $ widgetHold (categoryFormEdit $ cproductCategory pc0) $ updated $ categoryForm <$> catD
   patinationD <- patinationForm (cproductPatination pc0)
   authorsD <- authorsForm (cproductAuthors pc0)
   incrsD <- incrustationsForm (cproductIncrustations pc0)
   priceD <- priceForm (cproductPrice pc0)
   creationD <- dayCalendarValue <$> dayCalendarField "Дата изготовления" (def & dayCalendarConfig_inititialDay .~ cproductCreation pc0)
   locD <- mtextField "Место" (cproductLocation pc0)
   bookedD <- mtextField "Бронь" (cproductBooked pc0)
   groupD <- checkField "Выложен" (cproductInGroup pc0)
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

-- | Selection of price
-- TODO: other currencies
priceForm :: forall t m . MonadWidget t m => Price -> m (Dynamic t Price)
priceForm price = case price of
  PriceRub v -> fmap PriceRub <$> doubleField "Цена" v

-- | Form that allows to edit product category data
categoryFormEdit :: forall t m . MonadWidget t m => ProductCategoryData -> m (Dynamic t ProductCategoryData)
categoryFormEdit c = case c of
  PendantLeafData w h -> do
    pendantLeafWidth <- mdoubleField "Ширина" w
    pendantLeafHeight <- mdoubleField "Длина" h
    pure $ PendantLeafData <$> pendantLeafWidth <*> pendantLeafHeight
  PendantOtherData w h -> do
    pendantOtherWidth <- mdoubleField "Ширина" w
    pendantOtherHeight <- mdoubleField "Длина" h
    pure $ PendantOtherData <$> pendantOtherWidth <*> pendantOtherHeight
  NecklaceData w h -> do
    necklaceWidth <- mdoubleField "Ширина" w
    necklaceHeight <- mdoubleField "Длина" h
    pure $ NecklaceData <$> necklaceWidth <*> necklaceHeight
  EaringsData w h -> do
    earingsWidth <- mdoubleField "Ширина" w
    earingsHeight <- mdoubleField "Длина" h
    pure $ EaringsData <$> earingsWidth <*> earingsHeight
  BraceletData sub w h -> do
    mins <- mintField "Мин размер" w
    maxs <- mintField "Макс размер" h
    pure $ BraceletData <$> pure sub <*> mins <*> maxs
  RingData s -> do
    ringSize <- mintField "Размер" s
    pure $ RingData <$> ringSize
  HairData sub w h wt -> do
    earingsWidth <- mdoubleField "Ширина" w
    earingsHeight <- mdoubleField "Длина" h
    woodType <- case sub of
      HairPinWood -> mtextField "Тип дерева" wt
      _ -> pure $ pure Nothing
    pure $ HairData <$> pure sub <*> earingsWidth <*> earingsHeight <*> woodType
  BroochData sub w h -> do
    width <- mdoubleField "Ширина" w
    height <- mdoubleField "Длина" h
    pure $ BroochData <$> pure sub <*> width <*> height
  BookmarkData{..} -> do
    width <- mdoubleField "Ширина" bookmarkWidth
    height <- mdoubleField "Длина" bookmarkHeight
    pure $ BookmarkData <$> width <*> height
  GrandData -> pure $ pure GrandData

-- | Given the fixed category display form for setting cattegory specific data
categoryForm :: forall t m . MonadWidget t m => ProductCategory -> m (Dynamic t ProductCategoryData)
categoryForm c = case c of
  PendantLeaf -> do
    pendantLeafWidth <- mdoubleField "Ширина" Nothing
    pendantLeafHeight <- mdoubleField "Длина" Nothing
    pure $ PendantLeafData <$> pendantLeafWidth <*> pendantLeafHeight
  PendantOther -> do
    pendantOtherWidth <- mdoubleField "Ширина" Nothing
    pendantOtherHeight <- mdoubleField "Длина" Nothing
    pure $ PendantOtherData <$> pendantOtherWidth <*> pendantOtherHeight
  Necklace -> do
    necklaceWidth <- mdoubleField "Ширина" Nothing
    necklaceHeight <- mdoubleField "Длина" Nothing
    pure $ NecklaceData <$> necklaceWidth <*> necklaceHeight
  Earings -> do
    earingsWidth <- mdoubleField "Ширина" Nothing
    earingsHeight <- mdoubleField "Длина" Nothing
    pure $ EaringsData <$> earingsWidth <*> earingsHeight
  Bracelet sub -> do
    mins <- mintField "Мин размер" Nothing
    maxs <- mintField "Макс размер" Nothing
    pure $ BraceletData <$> pure sub <*> mins <*> maxs
  Ring -> do
    ringSize <- mintField "Размер" Nothing
    pure $ RingData <$> ringSize
  Hair sub -> do
    earingsWidth <- mdoubleField "Ширина" Nothing
    earingsHeight <- mdoubleField "Длина" Nothing
    woodType <- case sub of
      HairPinWood -> mtextField "Тип дерева" Nothing
      _ -> pure $ pure Nothing
    pure $ HairData <$> pure sub <*> earingsWidth <*> earingsHeight <*> woodType
  Brooch sub -> do
    width <- mdoubleField "Ширина" Nothing
    height <- mdoubleField "Длина" Nothing
    pure $ BroochData <$> pure sub <*> width <*> height
  Bookmark -> do
    width <- mdoubleField "Ширина" Nothing
    height <- mdoubleField "Длина" Nothing
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

toPatinationTag :: Patination -> PatinationTag
toPatinationTag p = case p of
  PatinationRainbow _ -> PatRainbow
  PatinationAmmonia -> PatAmmonia
  PatinationAmmoniaBlue -> PatAmmoniaBlue
  PatinationSulfur -> PatSulfur
  PatinationGreen -> PatGreen
  StainedGlassPaint _ -> PatStainedGlass

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
colorField :: forall t m . MonadWidget t m => Color -> m (Dynamic t Color)
colorField v = _dropdown_value <$> formGroupSelect "Цвет" v (pure colorLabels) def

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
stoneField :: forall t m . MonadWidget t m => Stone -> m (Dynamic t Stone)
stoneField v = _dropdown_value <$> formGroupSelect "Камень" v (pure stoneLabels) def

-- | Allows to input many values via dynamic count of simple fields
manyInputs :: forall t m k a . MonadWidget t m => [k] -> (Maybe k -> m (Dynamic t a)) -> m (Dynamic t [a])
manyInputs initialVals makeField = mdo
  let makeField' k v _ = row $ do
        a <- md10 $ makeField v
        delE <- md2 $ primaryButton "Удалить"
        pure (a, delE)
      initalMap = M.fromList $ [0 .. ] `zip` fmap Just initialVals
  tmap :: Dynamic t (Map Int (Dynamic t a, Event t ())) <- listWithKeyShallowDiff initalMap updE makeField'
  let valmap = joinDynThroughMap $ fmap fst <$> tmap :: Dynamic t (Map Int a)
      delmap = fmap snd <$> tmap :: Dynamic t (Map Int (Event t ()))
      delmap' = switchPromptlyDyn $ mergeMap <$> delmap :: Event t (Map Int ())
      delmap'' = fmap (const Nothing) <$> delmap' :: Event t (Map Int (Maybe (Maybe k)))
      maximum' :: [Int] -> Int
      maximum' [] = 0
      maximum' xs = maximum xs
      lastid = maximum' . M.keys <$> tmap :: Dynamic t Int
  addE <- row $ md1 (pure ()) >> md2 (primaryButton "Больше элементов")
  let addE' = attachPromptlyDynWith (\i _ -> [(i+1, Just Nothing)]) lastid addE :: Event t (Map Int (Maybe (Maybe k)))
      updE = delmap'' <> addE'
  pure $ fmap snd . sortBy (comparing fst) . M.toList <$> valmap
  where
    md1 = elClass "div" "col-md-1"
    md10 = elClass "div" "col-md-10"

-- | Part of form that allows dynamically input patination types with colors
patinationForm :: MonadWidget t m => Maybe Patination -> m (Dynamic t (Maybe Patination))
patinationForm mpat = do
  let initPatination = maybe NoPatination toPatinationTag mpat
  tagD <- _dropdown_value <$> formGroupSelect "Тип патины" initPatination (pure patinationLabels) def
  fmap join $ widgetHold (editForm mpat) $ updated $  makeForm <$> tagD
  where
    editForm Nothing = makeForm NoPatination
    editForm (Just p) = case p of
      PatinationRainbow colors -> do
        clrs <- fmap S.fromList <$> manyInputs (S.toList colors) (colorField . fromMaybe Red)
        pure $ Just . PatinationRainbow <$> clrs
      PatinationAmmonia -> pure . pure . Just $ PatinationAmmonia
      PatinationAmmoniaBlue -> pure . pure . Just $ PatinationAmmoniaBlue
      PatinationSulfur -> pure . pure . Just $ PatinationSulfur
      PatinationGreen -> pure . pure . Just $ PatinationGreen
      StainedGlassPaint colors -> do
        clrs <- fmap S.fromList <$> manyInputs (S.toList colors) (colorField . fromMaybe Red)
        pure $ Just . StainedGlassPaint <$> clrs
    makeForm tg = case tg of
      NoPatination -> pure $ pure Nothing
      PatRainbow -> do
        clrs <- fmap S.fromList <$> manyInputs [Red] (colorField . fromMaybe Red)
        pure $ Just . PatinationRainbow <$> clrs
      PatAmmonia -> pure . pure . Just $ PatinationAmmonia
      PatAmmoniaBlue -> pure . pure . Just $ PatinationAmmoniaBlue
      PatSulfur -> pure . pure . Just $ PatinationSulfur
      PatGreen -> pure . pure . Just $ PatinationGreen
      PatStainedGlass -> do
        clrs <- fmap S.fromList <$> manyInputs [Red] (colorField . fromMaybe Red)
        pure $ Just . StainedGlassPaint <$> clrs

-- | Names for author tags
authorTags :: Map AuthorCode Text
authorTags = [
    (AuthorOlga, "Шеффер")
  , (AuthorSveta, "Света")
  , (AuthorPolina, "Полина")
  , (AuthorOther, "Другой")
  ]

authorTagFromInfo :: AuthorInfo -> AuthorCode
authorTagFromInfo c = case c of
  KnownAuthor v -> v
  UnknownAuthor _ -> AuthorOther

-- | Part of form that allows to add authors dynamically
authorsForm :: forall t m . MonadWidget t m => Set (AuthorInfo, Double) -> m (Dynamic t (Set (AuthorInfo, Double)))
authorsForm authors = do
  formGroupLabel "Авторы" $ pure ()
  fmap S.fromList <$> manyInputs (S.toList authors) authorForm
  where
    authorForm :: Maybe (AuthorInfo, Double) -> m (Dynamic t (AuthorInfo, Double))
    authorForm mval = panel $ do
      authD <- authInfoForm $ fst <$> mval
      percent <- doubleField "Процент" $ maybe 100 snd mval
      pure $ (,) <$> authD <*> percent

    authInfoForm :: Maybe AuthorInfo -> m (Dynamic t AuthorInfo)
    authInfoForm mval = do
      let initAuthor = maybe AuthorOlga authorTagFromInfo mval
      tagD <- _dropdown_value <$> formGroupSelect "Автор" initAuthor (pure authorTags) def
      fmap join $ widgetHold (editForm mval) $ updated $ makeForm <$> tagD
      where
        editForm Nothing = makeForm AuthorOlga
        editForm (Just v) = case v of
          KnownAuthor c -> pure . pure . KnownAuthor $ c
          UnknownAuthor n -> do
            name <- textField "Имя" n
            pure $ UnknownAuthor <$> name
        makeForm tg = case tg of
          AuthorOlga -> pure . pure . KnownAuthor $ AuthorOlga
          AuthorSveta -> pure . pure . KnownAuthor $ AuthorSveta
          AuthorPolina -> pure . pure . KnownAuthor $ AuthorPolina
          AuthorOther -> do
            name <- textField "Имя" ""
            pure $ UnknownAuthor <$> name

-- | Tags incrustations types
data IncrustationTag =
    IncrGlass
  | IncrStone
  | IncrPearl
  | IncrBone
  | IncrOther
  deriving (Eq, Ord, Show, Read)

toIncrustationTag :: Incrustation -> IncrustationTag
toIncrustationTag v = case v of
  IncrustationGlass _ -> IncrGlass
  IncrustationStone _ -> IncrStone
  IncrustationPearl -> IncrPearl
  IncrustationBone -> IncrBone
  IncrustationOther -> IncrOther

incrustationLabels :: Map IncrustationTag Text
incrustationLabels = [
    (IncrGlass, "Стекло")
  , (IncrStone, "Камень")
  , (IncrPearl, "Жемчуг")
  , (IncrBone, "Кость")
  , (IncrOther, "Другое")
  ]

-- | Part of form that allows to add incrustations dynamically
incrustationsForm :: forall t m . MonadWidget t m => Set Incrustation -> m (Dynamic t (Set Incrustation))
incrustationsForm incrs = do
  formGroupLabel "Вставки" $ pure ()
  fmap S.fromList <$> manyInputs (S.toList incrs) incrustationForm
  where
    incrustationForm :: Maybe Incrustation -> m (Dynamic t Incrustation)
    incrustationForm mval = panel $ do
      let initVal = maybe IncrGlass toIncrustationTag mval
      tagD <- _dropdown_value <$> formGroupSelect "Вставка" initVal (pure incrustationLabels) def
      fmap join $ widgetHold (editForm mval) $ updated $ makeForm <$> tagD
      where
        editForm Nothing = makeForm IncrGlass
        editFomr (Just v) = case v of
          IncrustationGlass colors -> do
            clrs <- fmap S.fromList <$> manyInputs (S.toList colors) (colorField . fromMaybe Red)
            pure $ IncrustationGlass <$> clrs
          IncrustationStone stones -> do
            stns <- fmap S.fromList <$> manyInputs (S.toList stones) (stoneField . fromMaybe Quartz)
            pure $ IncrustationStone <$> stns
          IncrustationPearl -> pure . pure $ IncrustationPearl
          IncrustationBone -> pure . pure $ IncrustationBone
          IncrustationOther -> pure . pure $ IncrustationOther
        makeForm tg = case tg of
          IncrGlass -> do
            clrs <- fmap S.fromList <$> manyInputs [Red] (colorField . fromMaybe Red)
            pure $ IncrustationGlass <$> clrs
          IncrStone -> do
            stns <- fmap S.fromList <$> manyInputs [Quartz] (stoneField . fromMaybe Labrador)
            pure $ IncrustationStone <$> stns
          IncrPearl -> pure . pure $ IncrustationPearl
          IncrBone -> pure . pure $ IncrustationBone
          IncrOther -> pure . pure $ IncrustationOther
