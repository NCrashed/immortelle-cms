module Immortelle.CMS.Frontend.Utils(
    showt
  -- * Forms
  , mdoubleField
  , doubleField
  , doubleDynField
  , mintField
  , mtextField
  , textField
  , textField'
  , checkField
  , validate
  -- * Collections
  , manyInputs
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import Data.Text (Text, pack)
import Reflex.Dom
import Web.Reflex.Bootstrap

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | Helper to create bootstrap text input with label
formGroupText' :: MonadWidget t m => Text -> TextInputConfig t -> m (TextInput t)
formGroupText' labelText cfg = formGroup $ do
  mkLabel [ ("for", elemId)
          , ("class", "col-sm-2 control-label")] $ text labelText
  elClass "div" "col-sm-10" $ textInput cfg {
      _textInputConfig_attributes = ffor (_textInputConfig_attributes cfg) $ \m -> m `M.union` [
          ("class", "form-control")
        , ("id", elemId)
        , ("type", "text") ]
    }
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = "input" <> labelText

-- | Helper to parse optional double from labeled input field
mdoubleField :: forall t m . MonadWidget t m => Text -> Maybe Double -> m (Dynamic t (Maybe Double))
mdoubleField label mt = do
  let initText = maybe "" showt mt
  tinput <- formGroupText' label $ def & textInputConfig_initialValue .~ initText
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
  tinput <- formGroupText' label $ def & textInputConfig_initialValue .~ showt val
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

-- | Helper to parse double value from labeled input field and allows to forcelly update the value from outside
doubleDynField :: forall t m . MonadWidget t m => Text -> Dynamic t Double -> m (Dynamic t Double)
doubleDynField label valD = do
  val <- sample . current $ valD
  tinput <- formGroupText' label $ def
    & textInputConfig_initialValue .~ showt val
    & textInputConfig_setValue .~ fmap showt (updated valD)
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
  tinput <- formGroupText' label $ def & textInputConfig_initialValue .~ initText
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
  tinput <- formGroupText' label $ def & textInputConfig_initialValue .~ initText
  pure $ do
    val <- T.strip <$> _textInput_value tinput
    pure $ if T.null val then Nothing else Just val

-- | Helper to get text from labeled input field
textField :: forall t m . MonadWidget t m => Text -> Text -> m (Dynamic t Text)
textField label initText = fmap T.strip . _textInput_value <$> formGroupText' label (def & textInputConfig_initialValue .~ initText)

-- | Helper to get text from labeled input field
textField' :: forall t m . MonadWidget t m => Text -> TextInputConfig t -> m (Dynamic t Text)
textField' label cfg = fmap T.strip . _textInput_value <$> formGroupText' label cfg

-- | Helper for checkboxes
checkField :: forall t m . MonadWidget t m => Text -> Bool -> m (Dynamic t Bool)
checkField label val = formGroupLabel label $ divClass "checkbox" $ el "label" $ do
  res <- _checkbox_value <$> checkbox val def
  spanClass "checkbox-material" $ spanClass "check" $ pure ()
  pure res

-- | Pass through only those values that returns 'Right' in the predicate
validate :: forall t m a b . MonadWidget t m
  => (a -> Either Text b) -- ^ Validation predicate
  -> b -- ^ initial value for output dynamic
  -> Dynamic t a -- ^ stream of values that should be processed
  -> m (Dynamic t b) -- ^ resulted stream with valid values
validate f b0 da = do
  a0 <- sample . current $ da
  let validE = fforMaybe (updated da) $ \v -> case f v of
        Right b -> Just b
        _ -> Nothing
      initError = case f a0 of
        Left e -> danger e
        _ -> pure ()
  widgetHold initError $ ffor (updated da) $ \v -> case f v of
    Left e -> danger e
    _ -> pure ()
  let initVal = either (const b0) id $ f a0
  holdDyn initVal validE

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
