module Immortelle.CMS.State(
    DB(..)
  , emptyDB
  , GenProductId(..)
  , InsertProduct(..)
  , GetProduct(..)
  , DeleteProduct(..)
  , GetAuthorByCode(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Acid
import Data.SafeCopy
import Data.Map.Strict (Map)
import Immortelle.CMS.Types

import qualified Data.Map.Strict as M

data DB = DB {
  dbProducts :: !(Map ProductId Product)
, dbNextId   :: !ProductId
}
deriveSafeCopy 0 'base ''DB

emptyDB :: DB
emptyDB = DB {
  dbProducts = mempty
, dbNextId   = ProductId 0
}

genProductId :: Update DB ProductId
genProductId = do
  db <- get
  put db { dbNextId = ProductId . (+1) . unProductId . dbNextId $ db }
  pure $ dbNextId db

insertProduct :: Product -> Update DB ()
insertProduct pr = modify' $ \db -> db {
    dbProducts = M.insert (productId pr) pr $ dbProducts db
  }

getProduct :: ProductId -> Query DB (Maybe Product)
getProduct i = asks (M.lookup i . dbProducts)

deleteProduct :: ProductId -> Update DB ()
deleteProduct i = modify' $ \db -> db {
    dbProducts = M.delete i . dbProducts $ db
  }

getAuthorByCode :: AuthorCode -> Query DB (Maybe Author)
getAuthorByCode ac = pure $ case ac of
  AuthorOlga -> Just $ Author "Шеффер" AuthorOlga
  AuthorSveta -> Just $ Author "Света" AuthorSveta
  AuthorPolina -> Just $ Author "Полина" AuthorPolina
  AuthorOther -> Nothing

makeAcidic ''DB [
    'genProductId
  , 'insertProduct
  , 'getProduct
  , 'deleteProduct
  , 'getAuthorByCode
  ]
