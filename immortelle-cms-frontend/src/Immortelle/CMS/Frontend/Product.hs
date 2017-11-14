module Immortelle.CMS.Frontend.Product(
    productAddPage
  , productListPage
  ) where

import Control.Lens
import Control.Monad (join)
import Data.Bifunctor
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time
import Immortelle.CMS.API hiding (PagedList, Page)
import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Calendar
import Immortelle.CMS.Frontend.Client
import Immortelle.CMS.Frontend.Menu
import Immortelle.CMS.Frontend.Monad
import Immortelle.CMS.Frontend.Scroll
import Immortelle.CMS.Frontend.Utils
import Immortelle.CMS.Types
import Immortelle.CMS.VendorCode
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

productListPage :: forall t m . MonadFront t m => m (Event t CmsMenuItem)
productListPage = fmap switchPromptlyDyn . route $ listWidget
  where
    listWidget = mdo
      scrolledSuccess 500 $ ffor deletedE $ const "Изделие удалено!"
      actionE <- productList deletedE
      let editE = fforMaybe actionE $ \case
            ProductEdit p -> Just p
            _ -> Nothing
          delE = fforMaybe actionE $ \case
            ProductDelete i -> Just i
            _ -> Nothing
      delE' <- confirm def delE
      deletedE <- dangerResult =<< deleteProduct delE'
      let editR = Route $ editWidget <$> editE
      pure (never, editR)
    editWidget p = do
      backBtnE <- primaryButton "Назад"
      (updE, interProductD) <- productEdit p

      -- Trick to ask confirmation if fields where changed
      let confirmCfg = ConfirmConfig {
              _confirmConfigTitle = "Данные не сохранены. Точно перейти назад?"
            , _confirmConfigAcceptTitle = "Да"
            , _confirmConfigCancelTitle = "Нет"
            }
      backCheckedE <- confirm confirmCfg $ flip push backBtnE $ const $ do
        p' <- sample . current $ interProductD
        pure $ if p' == p then Nothing else Just ()
      let backUncheckedE = flip push backBtnE $ const $ do
            p' <- sample . current $ interProductD
            pure $ if p' == p then Just () else Nothing
          backE = leftmost [backCheckedE, backUncheckedE]

      pure (never, Route $ const listWidget <$> backE)

-- | Action that user requests in 'productList'
data ProductAction = ProductEdit Product | ProductDelete ProductId

-- | Widget that allows to view all products in table view with search
productList :: forall t m . MonadFront t m => Event t () -> m (Event t ProductAction)
productList reloadExternalE = do
  strD <- horizontalForm $ value <$> formGroupText "Поиск" def
  let reloadE = leftmost [
          () <$ updated strD
        , reloadExternalE
        ]
  selectsD :: Dynamic t [Event t ProductAction] <- renderListReload (Just 5) tableHeader productInfoWidget (loadProducts strD) reloadE
  pure $ switchPromptlyDyn $ fmap leftmost selectsD
  where
    tableHeader ma = tableHover mempty $ do
      thead . tr $ do
        th $ text "Артикул"
        th $ text "Название"
        th $ text "Категория"
        th $ text "Патина"
        th $ text "Авторы"
        th $ text "Инкрустация"
        th $ text "Сделан"
        th $ text "Находится"
        th $ text "Бронь"
        th $ text "В группе"
        th $ text "Цена"
      tbody ma

    productInfoWidget :: Product -> m (Event t ProductAction)
    productInfoWidget c@Product{..} = tr $ do
      td . text $ encodeVendorCode . productVendorCode $ c
      td . text $ productName
      td . text $ displayCategory . productCategoryFromData $ productCategory
      td . text $ maybe "Нет" displayPatination productPatination
      td . text $ T.intercalate " " . fmap (displayAuthor . fst) . S.toList $ productAuthors
      td . text $ T.intercalate " " . fmap displayIncrustationData . S.toList $ productIncrustations
      td . text $ maybe "" (T.pack . formatTime defaultTimeLocale "YYYY-MM-DD") $ productCreation
      td . text $ fromMaybe "" productLocation
      td . text $ fromMaybe "" productBooked
      td . text $ if productInGroup then "Да" else "Нет"
      td . text $ displayPrice productPrice
      -- action cell
      td $ do
        editE <- fmap (fmap $ const $ ProductEdit c) $ hrefTooltip TooltipBottom "Изменить" $ icon "edit"
        delE <- fmap (fmap $ const $ ProductDelete productId) $ hrefTooltip TooltipBottom "Удалить" $ icon "delete"
        pure $ leftmost [editE, delE]
    loadProducts :: Dynamic t Text -> Event t Page -> m (Event t (PagedList Product))
    loadProducts searchD epage = do
      let searchD' = ffor searchD $ \str -> if T.null str then Nothing else Just str
          ebody = attachPromptlyDynWith (\str n -> (str, PageInfo n 20)) searchD' epage
      mresE <- listProducts ebody
      handleDanger $ textifyResult <$> mresE

productCreateFirstField :: Text
productCreateFirstField = "product-create-first-field"

-- | Widget that contains form to create new product item
productCreate :: forall t m . MonadFront t m => m (Event t ProductId)
productCreate = mdo
  succDelayedE <- delay 0.3 succE
  scrolledSuccess 500 $ ffor succE $ \i -> "Новое изделие добавлено под ID: " <> showt i
  performEvent_ $ ffor succDelayedE $ const $ setFocus productCreateFirstField
  res :: Dynamic t (Event t ProductId, Dynamic t ProductCreate) <- widgetHold (body defaultProductCreate) $ body . resetProductCreate <$> reqE
  let reqD :: Dynamic t ProductCreate = join $ snd <$> res
      succE :: Event t ProductId = switchPromptlyDyn $ fst <$> res
      reqE :: Event t ProductCreate = tagPromptlyDyn reqD succE
  pure succE
  where
    body v = do
      prodDyn <- holdDyn v . fst =<< productCreateForm "Создать" v
      prodIdE <- dangerResult =<< insertProduct (updated prodDyn)
      pure (prodIdE, prodDyn)

toAuthorInfo :: Author -> AuthorInfo
toAuthorInfo Author{..} = case authorCode of
  AuthorOther -> UnknownAuthor authorName
  _ -> KnownAuthor authorCode

toCreateReq :: Product -> ProductCreate
toCreateReq Product{..} = ProductCreate {
    cproductName          = productName
  , cproductCategory      = productCategory
  , cproductPatination    = productPatination
  , cproductAuthors       = S.map (first toAuthorInfo) productAuthors
  , cproductIncrustations = productIncrustations
  , cproductPrice         = productPrice
  , cproductCreation      = productCreation
  , cproductLocation      = productLocation
  , cproductBooked        = productBooked
  , cproductInGroup       = productInGroup
  }

toPatchReq :: ProductCreate -> ProductPatch
toPatchReq ProductCreate{..} = ProductPatch {
    pproductName          = cproductName
  , pproductCategory      = cproductCategory
  , pproductPatination    = cproductPatination
  , pproductAuthors       = cproductAuthors
  , pproductIncrustations = cproductIncrustations
  , pproductPrice         = cproductPrice
  , pproductCreation      = cproductCreation
  , pproductLocation      = cproductLocation
  , pproductBooked        = cproductBooked
  , pproductInGroup       = cproductInGroup
  }

toAuthor :: AuthorInfo -> Author
toAuthor v = case v of
  KnownAuthor c -> case c of
    AuthorOlga -> Author "Шеффер" c
    AuthorSveta -> Author "Света" c
    AuthorPolina -> Author "Полина" c
    AuthorOther -> Author "" AuthorOther
  UnknownAuthor t -> Author t AuthorOther

applyPatch :: ProductPatch -> Product -> Product
applyPatch ProductPatch{..} p = p {
    productName          = pproductName
  , productCategory      = pproductCategory
  , productPatination    = pproductPatination
  , productAuthors       = S.map (first toAuthor) pproductAuthors
  , productIncrustations = pproductIncrustations
  , productPrice         = pproductPrice
  , productCreation      = pproductCreation
  , productLocation      = pproductLocation
  , productBooked        = pproductBooked
  , productInGroup       = pproductInGroup
  }

-- | Widget that contains form to edit existing product item
productEdit :: forall t m . MonadFront t m => Product -> m (Event t (), Dynamic t Product)
productEdit v = mdo
  scrolledSuccess 500 $ ffor succE $ const "Изделие обновлено!"
  (updE, prodD) <- productCreateForm "Сохранить" $ toCreateReq v
  succE <- dangerResult =<< updateProduct ((\cr -> (productId v, toPatchReq cr)) <$> updE)
  pure (succE, (`applyPatch` v) . toPatchReq <$> prodD)

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

-- | Displays form that allows to form create request for product.
--
-- Second return is intermidieate product after instant client side update.
productCreateForm :: forall t m . MonadFront t m => Text -> ProductCreate -> m (Event t ProductCreate, Dynamic t ProductCreate)
productCreateForm submText pc0 = horizontalForm $ do
   nameD <- textField' "Имя изделия" (def
      & textInputConfig_initialValue .~ cproductName pc0
      & textInputConfig_attributes .~ pure [("id", productCreateFirstField)])
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
   submitE <- submitButton submText
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
   pure (tagPromptlyDyn requestD submitE, requestD)

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
  RingData s adj -> do
    ringSize <- mdoubleField "Размер" s
    ringAdj <- checkField "Настраиваемый" adj
    pure $ RingData <$> ringSize <*> ringAdj
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
    ringSize <- mdoubleField "Размер" Nothing
    ringAdj <- checkField "Настраиваемый" False
    pure $ RingData <$> ringSize <*> ringAdj
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
  , (PatAmmoniaBlue, "Аммиак голубой")
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
  , (White, "Белый")
  , (Black, "Чёрный")
  , (Pink, "Розовый")
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
  | IncrGlassDichroic
  | IncrStone
  | IncrPearlDark
  | IncrPearlBright
  | IncrPorcelain
  | IncrBone
  | IncrOther
  deriving (Eq, Ord, Show, Read)

toIncrustationTag :: IncrustationData -> IncrustationTag
toIncrustationTag v = case v of
  IncrustationDataGlass _ -> IncrGlass
  IncrustationDataGlassDichroic _ -> IncrGlassDichroic
  IncrustationDataStone _ -> IncrStone
  IncrustationDataPearl pt -> case pt of
    PearlDark -> IncrPearlDark
    PearlBright -> IncrPearlBright
  IncrustationDataPorcelain _ -> IncrPorcelain
  IncrustationDataBone -> IncrBone
  IncrustationDataOther _ -> IncrOther

incrustationLabels :: Map IncrustationTag Text
incrustationLabels = [
    (IncrGlass, "Стекло")
  , (IncrGlassDichroic, "Стекло дихроическое")
  , (IncrStone, "Камень")
  , (IncrPearlBright, "Жемчуг светлый")
  , (IncrPearlDark, "Жемчуг тёмный")
  , (IncrPorcelain, "Фарфор")
  , (IncrBone, "Кость")
  , (IncrOther, "Другое")
  ]

-- | Part of form that allows to add incrustations dynamically
incrustationsForm :: forall t m . MonadWidget t m => Set IncrustationData -> m (Dynamic t (Set IncrustationData))
incrustationsForm incrs = do
  formGroupLabel "Вставки" $ pure ()
  fmap S.fromList <$> manyInputs (S.toList incrs) incrustationForm
  where
    incrustationForm :: Maybe IncrustationData -> m (Dynamic t IncrustationData)
    incrustationForm mval = panel $ do
      let initVal = maybe IncrGlass toIncrustationTag mval
      tagD <- _dropdown_value <$> formGroupSelect "Вставка" initVal (pure incrustationLabels) def
      fmap join $ widgetHold (editForm mval) $ updated $ makeForm <$> tagD
      where
        editForm Nothing = makeForm IncrGlass
        editForm (Just v) = case v of
          IncrustationDataGlass colors -> do
            clrs <- fmap S.fromList <$> manyInputs (S.toList colors) (colorField . fromMaybe Red)
            pure $ IncrustationDataGlass <$> clrs
          IncrustationDataGlassDichroic colors -> do
            clrs <- fmap S.fromList <$> manyInputs (S.toList colors) (colorField . fromMaybe Red)
            pure $ IncrustationDataGlassDichroic <$> clrs
          IncrustationDataStone stones -> do
            stns <- fmap S.fromList <$> manyInputs (S.toList stones) (stoneField . fromMaybe Quartz)
            pure $ IncrustationDataStone <$> stns
          IncrustationDataPearl pt -> pure . pure $ IncrustationDataPearl pt
          IncrustationDataPorcelain colors -> do
            clrs <- fmap S.fromList <$> manyInputs (S.toList colors) (colorField . fromMaybe White)
            pure $ IncrustationDataPorcelain <$> clrs
          IncrustationDataBone -> pure . pure $ IncrustationDataBone
          IncrustationDataOther other -> do
            name <- textField "Другое" other
            pure $ IncrustationDataOther <$> name
        makeForm tg = case tg of
          IncrGlass -> do
            clrs <- fmap S.fromList <$> manyInputs [Red] (colorField . fromMaybe Red)
            pure $ IncrustationDataGlass <$> clrs
          IncrGlassDichroic -> do
            clrs <- fmap S.fromList <$> manyInputs [Red] (colorField . fromMaybe Red)
            pure $ IncrustationDataGlassDichroic <$> clrs
          IncrStone -> do
            stns <- fmap S.fromList <$> manyInputs [Quartz] (stoneField . fromMaybe Labrador)
            pure $ IncrustationDataStone <$> stns
          IncrPearlDark -> pure . pure $ IncrustationDataPearl PearlDark
          IncrPearlBright -> pure . pure $ IncrustationDataPearl PearlBright
          IncrPorcelain -> do
            clrs <- fmap S.fromList <$> manyInputs [] (colorField . fromMaybe White)
            pure $ IncrustationDataPorcelain <$> clrs
          IncrBone -> pure . pure $ IncrustationDataBone
          IncrOther -> do
            name <- textField "Другое" ""
            pure $ IncrustationDataOther <$> name
