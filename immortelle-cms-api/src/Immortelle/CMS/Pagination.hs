{-# OPTIONS_GHC -fno-warn-orphans #-}
module Immortelle.CMS.Pagination(
  -- * API types
    PageParam
  , Page
  , PageSizeParam
  , PageSize
  , PageInfo(..)
  -- ** Helpers
  , PagedList(..)
  , filterPagedList
  , pagedListLength
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.WithField
import Data.Monoid
import Data.SafeCopy
import GHC.Generics
import Immortelle.CMS.Aeson as AS
import Servant.API

import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | Query parameter that carries pagination page number
type PageParam = QueryParam "page" Page
-- | A page number
type Page = Word

-- | Query parameter that carries pagination page size value
type PageSizeParam = QueryParam "size" PageSize
-- | Number of items on a page
type PageSize = Word

-- | Page info that can be placed in body
data PageInfo = PageInfo {
  pageInfoPage :: !Page     -- ^ Page number
, pageInfoSize :: !PageSize -- ^ Page size
} deriving (Generic)
deriveJSON (AS.exactPrefixOptions "pageInfo") ''PageInfo
deriveSafeCopy 0 'base ''PageInfo

-- | Helper to display value via show method
showt :: Show a => a -> T.Text
showt = T.pack . show

-- | Helper to execute text parser
readt :: T.Reader a -> T.Text -> Either T.Text a
readt p t = case p t of
  Left e -> Left $ T.pack e
  Right (a, r) -> if T.null r then Right a else Left $ "Unexpected input: " <> r

instance ToHttpApiData PageInfo where
  toUrlPiece PageInfo{..} = showt pageInfoPage <> "," <> showt pageInfoSize

instance FromHttpApiData PageInfo where
  parseUrlPiece t = PageInfo
    <$> readt T.decimal p
    <*> readt T.decimal (T.drop 1 s)
    where
      (p, s) = T.break (== ',') t

-- | Collection of 'a' and additional
-- page info.
data PagedList a = PagedList {
  pagedListItems :: ![a] -- ^ Payload
, pagedListPages :: !Word -- ^ Count of available pages
} deriving (Generic, Show)

deriveSafeCopy 0 'base ''PagedList

instance (ToJSON a) => ToJSON (PagedList a) where
  toJSON PagedList{..} = object [
      "items" .= pagedListItems
    , "pages" .= pagedListPages
    ]

instance (FromJSON a) => FromJSON (PagedList a) where
  parseJSON (Object o) = PagedList
    <$> o .: "items"
    <*> o .: "pages"
  parseJSON _ = mzero

instance Functor PagedList where
  fmap f p = p {
      pagedListItems = f <$> pagedListItems p
    }
  {-# INLINE fmap #-}

instance Foldable PagedList where
  foldMap f = foldMap f . pagedListItems
  {-# INLINE foldMap #-}

instance Traversable PagedList where
  traverse f a = fmap (\xs -> PagedList xs (pagedListPages a)) . traverse f . pagedListItems $ a
  {-# INLINE traverse #-}

-- | Leave elements in paged list that are passes through predicate
filterPagedList :: (a -> Bool) -> PagedList a -> PagedList a
filterPagedList f p = p { pagedListItems = filter f $ pagedListItems p}

-- | Return count of elements in paged list
pagedListLength :: PagedList a -> Int
pagedListLength = length . pagedListItems
