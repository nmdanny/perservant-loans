{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime,getCurrentTime)
import Control.Monad.Except (MonadError,throwError)
import Data.Aeson
import GHC.Generics                (Generic)
import Data.Typeable (Typeable)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Database.Persist.Sql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)

import Servant.Swagger
import Data.Swagger (ToSchema, ToParamSchema)

import Config

-- | This module defines the models of our app, using persistent to model and persist the data.

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    email Text
    password Text
    createDate UTCTime default=CURRENT_DATE
    UniqueEmail email
    UniqueName name
    deriving Show Generic Typeable

Loan json
   amount Int
   lender UserId
   borrower UserId
   startDate UTCTime default=CURRENT_DATE
   endDate UTCTime Maybe
   deriving Show Generic Typeable
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll


-- | We use these data types to control which table fields are exposed to the users/required from them.

data UserPOST = UserPOST
  { userPostName :: Text
  , userPostEmail:: Text
  , userPostPassword :: Text
  } deriving (Eq,Show,Generic,Typeable)
data UserGET = UserGET
  { userGetName :: Text
  , userGetEmail:: Text
  , userGetCreateDate :: UTCTime
  , userId :: Key User
  } deriving (Eq,Show,Generic,Typeable)


-- we provide them with custom ToJSON/FromJSON, as we don't want users of the API
-- to type long names such as 'userPostName' etc..
instance ToJSON UserGET where
  toJSON (UserGET name email date uid) = object
                                     ["name" .= name
                                     ,"email".= email
                                     ,"createDate" .= date
                                     ,"id" .= uid
                                     ]
instance FromJSON UserPOST where
  parseJSON (Object v) = UserPOST  <$>
                       v .: "name" <*>
                       v .: "email"<*>
                       v .: "password"

-- | Process a user post data, resulting in just an user if its valid, otherwise resulting in nothing..
postUserDTO :: UserPOST -> UTCTime -> User
postUserDTO (UserPOST name email pass) time = User name email pass time

-- | Create a user get data from a user, exposing fields that the user would be interested in.
getUserDTO :: Entity User -> UserGET
getUserDTO (Entity k (User name email pass createDate)) = UserGET name email createDate k
