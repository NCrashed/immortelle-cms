module Immortelle.CMS.Frontend.Calendar(
    dayCalendarField
  , DayCalendarConfig(..)
  , DayCalendar(..)
  , dayCalendarConfig_inititialDay
  ) where

import Control.Lens.TH
import Control.Monad.IO.Class
import Data.Default
import Data.JSString (JSString, pack)
import Data.Monoid
import Data.Text (Text)
import Data.Time
import Reflex.Dom

import qualified Data.Text as T

import Web.Reflex.Bootstrap.Form
import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Utils

data DayCalendarConfig = DayCalendarConfig {
  _dayCalendarConfig_inititialDay :: Maybe Day
}
makeLenses ''DayCalendarConfig

instance Default DayCalendarConfig where
  def = DayCalendarConfig {
      _dayCalendarConfig_inititialDay = Nothing
    }

data DayCalendar t = DayCalendar {
  dayCalendarValue :: Dynamic t (Maybe Day)
}

foreign import javascript unsafe "$('#'+$1)['bootstrapMaterialDatePicker']({ 'time': false, 'format' : 'YYYY-MM-DD' });" js_makeDatePicker :: JSString -> IO ()

makeDatePicker :: MonadIO m => Text -> m ()
makeDatePicker i = liftIO $ js_makeDatePicker $ pack . T.unpack $ i

-- | Make a text field that is filled with material Bootstrap calendar widget
dayCalendarField :: forall t m . MonadWidget t m => Text -> DayCalendarConfig -> m (DayCalendar t)
dayCalendarField label cfg = do
  i <- genId
  let inputId = "day-calendar" <> showt i
      initText = maybe "" (T.pack . formatTime defaultTimeLocale "YYYY-MM-DD") $ _dayCalendarConfig_inititialDay cfg
  tinput <- formGroup $ do
    mkLabel [ ("for", inputId)
            , ("class", "col-sm-2 control-label")] $ text label
    elClass "div" "col-sm-10" $ textInput TextInputConfig {
        _textInputConfig_inputType = "text"
      , _textInputConfig_initialValue = initText
      , _textInputConfig_setValue = never
      , _textInputConfig_attributes = pure [
            ("class", "form-control")
          , ("id", inputId)
          , ("type", "text")
          ]
      }
  buildE <- delay 0.3 =<< getPostBuild
  performEvent_ $ ffor buildE $ const $ makeDatePicker inputId
  let mres = do
        val <- T.strip <$> _textInput_value tinput
        let parseDay :: Text -> Maybe Day
            parseDay = parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) . T.unpack
        pure $ if T.null val then Right Nothing else case parseDay val of
          Nothing -> Left $ "Ошибка в чтении даты!"
          Just v -> Right . Just $ v
  widgetHold (pure ()) $ ffor (updated mres) $ \case
    Left er -> danger er
    _ -> pure ()
  resD <- holdDyn Nothing $ fforMaybe (updated mres) $ \case
    Right v -> Just v
    _ -> Nothing
  pure DayCalendar {
      dayCalendarValue = resD
    }
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
