module Immortelle.CMS.Types.Author.V1(
    Author(..)
  , AuthorCode(..)
  , displayAuthor
  ) where

import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics
import Immortelle.CMS.Aeson

-- | Codes of authors in vendoc code
data AuthorCode =
    AuthorOlga
  | AuthorSveta
  | AuthorPolina
  | AuthorOther
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''AuthorCode
deriveJSON defaultOptions ''AuthorCode

-- | Author of product
data Author = Author {
  authorName :: Text
, authorCode :: AuthorCode
} deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 0 'base ''Author
deriveJSON defaultOptions ''Author

displayAuthor :: Author -> Text
displayAuthor = authorName
