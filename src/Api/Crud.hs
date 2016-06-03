{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Crud where

import Data.Text (Text)

import           Control.Lens
import           Data.Aeson (FromJSON,ToJSON)
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS

import           Config                      (Config (..), App(..), runDb)

-- TODO: Understand if the following is needed

-- | Represents an entity that can keyed. It is isomorphic to Persistent's Key.
--newtype Keyable a = Keyable { getKey :: Int64} deriving (FromJSON,ToJSON,FromHttpApiData)

-- | Defines an isomorphism between (Keyable a) and (ToBackendKey SqlBackend a)
--_Keyable :: ToBackendKey SqlBackend a => Iso' (Keyable a) (Key a)
--_Keyable = iso (toSqlKey . getKey) (Keyable . fromSqlKey)


-- | This module contains a generic API type that supports CRUD operations over a persistent entity:
--   - Create (POST)
--   - Read (GET)
--   - Update (PUT)
--   - Remove (DELETE)
-- Where the resource is indexed by (Key a), the resource without the index is just 'a', the
-- resource with its key is (Entity a).
-- You can use the included CrudAPI (and crudServer), or you can mix-match the prebuilt handlers/endpoints with ones of your own.


-- | A CRUD api for a persistent record of type 'a'.
type CrudAPI a =
          ReqBody '[JSON] a :> Post '[JSON] Int64  -- post
    :<|>  Capture "id" (Key a) :> Get '[JSON] (Entity a) -- get
    :<|>  Capture "id" (Key a) :> DeleteNoContent '[JSON] NoContent -- delete
    :<|>  Capture "id" (Key a) :> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- put

-- | A boilerplate type alias that says that the 'val' can be a persistent entity.
type Persistable val = (PersistEntityBackend val ~ SqlBackend, PersistEntity val,ToBackendKey SqlBackend val)

crudServer :: Persistable a => ServerT (CrudAPI a) App
crudServer = crudPost :<|> crudGet :<|> crudDelete :<|> crudPut

crudPost :: Persistable a => a -> App Int64
crudPost a = do
  newKey <- runDb (insert a)
  return $ fromSqlKey newKey

crudGet :: Persistable a => Key a -> App (Entity a)
crudGet k = do
  mbA <- runDb $ get k
  case mbA of
    Nothing -> throwError err404
    Just a -> return $ Entity k a

crudDelete :: Persistable a => Key a -> App NoContent
crudDelete k = do
  runDb $ delete k
  return NoContent

crudPut :: Persistable a => Key a -> a -> App NoContent
crudPut k a = do
  exists <- runDb $ get k
  case exists of
    Nothing -> throwError err404
    Just _ -> runDb $ replace k a >> return NoContent
