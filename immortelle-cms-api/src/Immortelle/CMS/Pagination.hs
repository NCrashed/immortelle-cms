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
import GHC.Generics
import Servant.API

import Immortelle.CMS.Aeson as AS

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

-- | Collection of 'a' with attached ids of type 'i' and additional
-- page info.
data PagedList i a = PagedList {
  pagedListItems :: ![WithId i a] -- ^ Payload
, pagedListPages :: !Word -- ^ Count of available pages
} deriving (Generic, Show)

instance (ToJSON i, ToJSON a) => ToJSON (PagedList i a) where
  toJSON PagedList{..} = object [
      "items" .= pagedListItems
    , "pages" .= pagedListPages
    ]

instance (FromJSON i, FromJSON a) => FromJSON (PagedList i a) where
  parseJSON (Object o) = PagedList
    <$> o .: "items"
    <*> o .: "pages"
  parseJSON _ = mzero

instance Functor (PagedList i) where
  fmap f p = p {
      pagedListItems = fmap f <$> pagedListItems p
    }
  {-# INLINE fmap #-}

instance Foldable (PagedList i) where
  foldMap f = foldMap (f . (\(WithField _ a) -> a)) . pagedListItems
  {-# INLINE foldMap #-}

instance Traversable (PagedList i) where
  traverse f a = fmap (\xs -> PagedList xs (pagedListPages a)) . traverse (\(WithField i a) -> WithField i <$> f a) . pagedListItems $ a
  {-# INLINE traverse #-}

-- | Leave elements in paged list that are passes through predicate
filterPagedList :: (a -> Bool) -> PagedList i a -> PagedList i a
filterPagedList f p = p { pagedListItems = filter (f . (\(WithField _ a) -> a)) $ pagedListItems p}

-- | Return count of elements in paged list
pagedListLength :: PagedList i a -> Int
pagedListLength = length . pagedListItems
