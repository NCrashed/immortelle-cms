module Immortelle.CMS.State(
    DB(..)
  , GenProductId(..)
  , InsertProduct(..)
  , GetProduct(..)
  , DeleteProduct(..)
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

makeAcidic ''DB [
    'genProductId
  , 'insertProduct
  , 'getProduct
  , 'deleteProduct
  ]
