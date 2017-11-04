module Immortelle.CMS.Frontend.Product(
    productAddPage
  ) where

import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Menu
import Reflex.Dom

productAddPage :: forall t m . MonadWidget t m => Dynamic t SimpleToken -> m (Event t CmsMenuItem)
productAddPage _ = pure never
