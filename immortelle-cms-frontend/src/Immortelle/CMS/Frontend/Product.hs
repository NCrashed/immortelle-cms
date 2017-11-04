module Immortelle.CMS.Frontend.Product(
    productAddPage
  ) where

import Immortelle.CMS.Frontend.Auth
import Immortelle.CMS.Frontend.Menu
import Immortelle.CMS.Frontend.Monad
import Reflex.Dom

productAddPage :: forall t m . MonadFront t m => m (Event t CmsMenuItem)
productAddPage = pure never
