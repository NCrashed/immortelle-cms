module Immortelle.CMS.Frontend(
    cmsFrontend
  ) where

import Data.Proxy
import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Menu
import Immortelle.CMS.Frontend.Product
import Reflex
import Reflex.Dom
import Web.Reflex.Bootstrap

-- | Main widget of CMS
cmsFrontend :: MonadWidget t m => m ()
cmsFrontend = container $ mdo
  mtokenD <- authWidget 300 logoutE
  logoutE <- switchPromptlyDyn <$> startWithJust (pure never) bodyWidget mtokenD
  pure ()
  where
  bodyWidget tokenD = menuWidget (Proxy :: Proxy CmsMenu) ProductAddPage [
      (ProductAddPage, productAddPage tokenD)
    ]

-- | Make a new dynamic that holds only 'Just' values from original dynamic
forJusts :: (Reflex t, MonadHold t m) => a -> Dynamic t (Maybe a) -> m (Dynamic t a)
forJusts a0 = holdDyn a0 . fmapMaybe id . updated

-- | Start applying widget from first 'Just' value of dynamic and use only 'Just' values so on.
startWithJust :: MonadWidget t m => m b -> (Dynamic t a -> m b) -> Dynamic t (Maybe a) -> m (Dynamic t b)
startWithJust mb0 mf d = do
  mv0 <- sample . current $ d
  case mv0 of
    Just v0 -> do
      d' <- forJusts v0 d
      pure <$> mf d'
    Nothing -> do
      firstE <- headE . fmapMaybe id . updated $ d
      widgetHold mb0 $ ffor firstE $ \a0 -> do
        d' <- forJusts a0 d
        mf d'
