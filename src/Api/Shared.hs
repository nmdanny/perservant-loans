{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ExplicitForAll #-}


module Api.Shared () where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson                  (toJSON)
import           Data.Aeson.Encode (encode)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Time                   (getCurrentTime)
import           Data.Typeable               (Typeable)
import           Database.Esqueleto          ((^.))
import qualified Database.Esqueleto          as E
import           Database.Persist.Postgresql (Entity (..), Key, fromSqlKey, get,
                                              getBy, insert, selectFirst,
                                              selectList, (==.))
import           GHC.Generics                (Generic)
import           Network.Wai                 (Application)

import           Data.Swagger                (ToParamSchema, ToSchema)
import           Servant
import           Servant.JS
import           Servant.Swagger

import           Api.Crud                    (CrudAPI, crudServer)
import           Config                      (App (..), Config (..), runDb)
