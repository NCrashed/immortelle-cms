module Immortelle.CMS.Frontend.Scroll(
    scrollTo
  , scrolledSuccess
  , setFocus
  ) where

import Control.Monad.IO.Class
import Data.JSString (JSString, pack)
import Data.Monoid
import Data.Text (Text)
import Reflex.Dom
import Web.Reflex.Bootstrap

import qualified Data.Text as T

foreign import javascript unsafe "$(window)['scrollTo']($('#'+$1), $2);"
  js_scrollTo :: JSString -> Int -> IO ()

-- | Scroll to given element with id within given amount of time
scrollTo :: MonadIO m
  => Text -- ^ ID of element
  -> Int -- ^ Number of milliseconds
  -> m ()
scrollTo i ms = liftIO $ js_scrollTo (pack . T.unpack $ i) ms

-- | Display success panel with text and scroll to it when event fires
scrolledSuccess :: forall t m . MonadWidget t m
  => Int -- ^ Number of milliseconds for scrolling animation
  -> Event t Text -- ^ Event with content text
  -> m ()
scrolledSuccess dt et = do
  i <- genId
  let it = "success-" <> showt i
      attrs = [("class", "alert alert-success"), ("id", it)]
  widgetHold (pure ()) $ ffor et $ elAttr "div" attrs . text
  et' <- delay 0.3 et
  performEvent_ $ ffor et' $ const $ scrollTo it dt

foreign import javascript unsafe "$('#'+$1)['focus']();"
  js_setFocus :: JSString -> IO ()

-- | Set focus to input element
setFocus :: MonadIO m
  => Text -- ^ ID of element
  -> m ()
setFocus i = liftIO $ js_setFocus (pack . T.unpack $ i)
