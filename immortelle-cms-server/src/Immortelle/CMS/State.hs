module Immortelle.CMS.State(
    DB(..)
  , emptyDB
  , GenProductId(..)
  , InsertProduct(..)
  , GetProduct(..)
  , DeleteProduct(..)
  , GetAuthorByCode(..)
  , ListProducts(..)
  -- Auth inherited
  , GetUserImpl(..)
  , GetUserImplByLogin(..)
  , ListUsersPaged(..)
  , GetUserImplPermissions(..)
  , DeleteUserPermissions(..)
  , InsertUserPerm(..)
  , InsertUserImpl(..)
  , ReplaceUserImpl(..)
  , DeleteUserImpl(..)
  , HasPerm(..)
  , GetFirstUserByPerm(..)
  , SelectUserImplGroups(..)
  , ClearUserImplGroups(..)
  , InsertAuthUserGroup(..)
  , InsertAuthUserGroupUsers(..)
  , InsertAuthUserGroupPerms(..)
  , GetAuthUserGroup(..)
  , ListAuthUserGroupPermissions(..)
  , ListAuthUserGroupUsers(..)
  , ReplaceAuthUserGroup(..)
  , ClearAuthUserGroupUsers(..)
  , ClearAuthUserGroupPerms(..)
  , DeleteAuthUserGroup(..)
  , ListGroupsPaged(..)
  , SetAuthUserGroupName(..)
  , SetAuthUserGroupParent(..)
  , InsertSingleUseCode(..)
  , SetSingleUseCodeUsed(..)
  , GetUnusedCode(..)
  , InvalidatePermanentCodes(..)
  , SelectLastRestoreCode(..)
  , InsertUserRestore(..)
  , FindRestoreCode(..)
  , ReplaceRestoreCode(..)
  , FindAuthToken(..)
  , FindAuthTokenByValue(..)
  , InsertAuthToken(..)
  , ReplaceAuthToken(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Acid
import Data.Map.Strict (Map)
import Data.Maybe
import Data.SafeCopy
import Data.Text (Text)
import Immortelle.CMS.Pagination
import Immortelle.CMS.Types
import Immortelle.CMS.VendorCode

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Servant.Server.Auth.Token.Acid.Schema as A

data DB = DB {
  dbProducts :: !(Map ProductId Product)
, dbNextId   :: !ProductId
, dbAuth     :: !A.Model
}
deriveSafeCopy 0 'base ''DB

emptyDB :: DB
emptyDB = DB {
  dbProducts = mempty
, dbNextId   = ProductId 0
, dbAuth     = A.newModel
}

instance A.HasModelRead DB where
  askModel = dbAuth

instance A.HasModelWrite DB where
  putModel db m = db { dbAuth = m }

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

listProducts :: Maybe Text -> Maybe PageInfo -> Query DB (PagedList Product)
listProducts mtext mpage = do
  let p = fromIntegral $ maybe 0 pageInfoPage mpage
      s = fromIntegral $ maybe 20 pageInfoSize mpage
      searchFilter = case mtext of
        Nothing -> const True
        Just rawQuery -> \p@Product{..} -> or [
            contains $ encodeVendorCode . productVendorCode $ p
          , contains productName
          , contains $ displayCategory . productCategoryFromData $ productCategory
          , contains $ maybe "" displayPatination productPatination
          , contains $ T.intercalate " " . fmap (displayAuthor . fst) . S.toList $ productAuthors
          , contains $ T.intercalate " " . fmap displayIncrustationData . S.toList $ productIncrustations
          , contains $ fromMaybe "" productLocation
          , contains $ fromMaybe "" productBooked
          ]
          where
            contains a = case t `T.breakOn` T.toLower a of
              (_, "") -> False
              _ -> True
            t = T.toLower . T.strip $ rawQuery
  products <- asks $ filter searchFilter . M.elems . dbProducts
  let paged = take s . drop (p*s) $ products
  pure $ PagedList {
      pagedListPages = ceiling $ fromIntegral (length products) / fromIntegral s
    , pagedListItems = paged
    }


A.deriveQueries ''DB
makeAcidic ''DB $ [
    'genProductId
  , 'insertProduct
  , 'getProduct
  , 'deleteProduct
  , 'getAuthorByCode
  , 'listProducts
  ] ++ A.acidQueries
