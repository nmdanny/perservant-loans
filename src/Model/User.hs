{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.User where

import           Control.Monad              (join, sequence)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Crypto.PasswordStore       (makePassword)
import           Data.Aeson
import           Data.Aeson.Types           (Options (..))
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe)
import           Data.Swagger               (ToParamSchema, ToSchema)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Typeable              (Typeable)
import           Database.Persist           ((==.), (||.))
import           Database.Persist.Sql
import           Database.Persist.TH        (mkMigrate, mkPersist,
                                             persistLowerCase, share,
                                             sqlSettings)
import           GHC.Generics               (Generic)
import           Servant.Server             (ServantErr, err404, errBody)
import           Servant.Swagger


import           Config
import qualified Model

newtype UserId = UserId Int64 deriving (Show,Eq,Generic,Typeable,Num,Ord,Bounded)

unUserId :: UserId -> Int64
unUserId (UserId i) = i

data User = User {
    userName :: Text
  , userEmail :: Text
  , userPassword :: Text
  , userCreateDate :: UTCTime
  , userUpdateDate :: UTCTime
  } deriving (Show,Generic,Typeable)

-- | Converts a basic 'User' type to a model user type.
userToModel :: User -> Model.User
userToModel User{..} = Model.User userName userEmail (encodeUtf8 userPassword) userCreateDate userUpdateDate

-- | Converts a model 'User' type to a basic user type.
modelToUser :: Model.User -> User
modelToUser Model.User{..} = User userName userEmail (decodeUtf8 userPassword) userCreateDate userUpdateDate

-- | A read-only user datatype that is meant to be sent from the server to the clients, containing
-- public data.
data ReadUser = ReadUser
  { readUserId         :: UserId
  , readUserName       :: Text
  , readUserEmail      :: Text
  , readUserCreateDate :: UTCTime
  , readUserUpdateDate :: UTCTime
  } deriving (Show,Eq,Generic,Typeable)

-- | A write-only user datatype sent from the client to the server, containing sensitive data.
-- meant for inserting a new user with the given credentials.
data WriteUser = WriteUser
  {
    writeUserName     :: Text
  , writeUserEmail    :: Text
  , writeUserPassword :: Text
  } deriving (Show,Eq,Generic,Typeable)

-- | A write-only user datatype sent from the client to the server, containing sensitive data.
-- meant for updating an already existing user based on his id.
data UpdateUser = UpdateUser
  {
    updateUserId       :: UserId
  , updateUserName     :: Maybe Text
  , updateUserEmail    :: Maybe Text
  , updateUserPassword :: Maybe Text
  } deriving (Show,Eq,Generic,Typeable)

-- | Inserts a 'WriteUser' to the database, updating the timestamps and encrypting the password on the way.
doWriteUser :: (MonadIO m,MonadReader Config m) => WriteUser -> m (Key Model.User)
doWriteUser WriteUser{..} = do
    curTime <- liftIO getCurrentTime
    encryptedPwd <- encryptText writeUserPassword 20
    runDb $ insert (Model.User writeUserName writeUserEmail encryptedPwd curTime curTime)

-- | Updates a user in the DB given a 'WriteUser'
-- If no such user exists, an error will be thrown.
-- TODO: Find a way to make this cleaner.
doUpdateUser :: (MonadIO m,MonadReader Config m,MonadError ServantErr m) => UpdateUser -> m (Key Model.User)
doUpdateUser UpdateUser{..} = do
  newTime <- liftIO getCurrentTime
  let k :: Key Model.User
      k = toSqlKey (unUserId updateUserId)
  hashedNewPwd <- sequence $ updateUserPassword >>= \txtPwd -> return $ encryptText txtPwd 20
  updateTime <- liftIO getCurrentTime
  curUser <- runDb $ get k
  case curUser of
    Nothing -> throwError err404 {errBody = "No user with the given ID exists."}
    Just user@Model.User{..} -> do
      let newUser = user { Model.userName = fromMaybe userName updateUserName
                         , Model.userEmail = fromMaybe userEmail updateUserEmail
                         , Model.userPassword = fromMaybe userPassword hashedNewPwd
                         , Model.userUpdateDate = newTime
                         }
      runDb $ replace k newUser
      return k

-- | Converts a DB user Entity to a 'readUser', hiding sensitive data.
readUser :: Entity Model.User -> ReadUser
readUser (Entity k Model.User{..}) = ReadUser (UserId $ fromSqlKey k) userName userEmail userCreateDate userUpdateDate

-- JSON Instances
instance ToJSON UserId where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True}
instance FromJSON UserId where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True}

instance ToJSON User where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 4}
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 4}

instance ToJSON ReadUser where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 4}
instance ToJSON WriteUser where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 5}
instance ToJSON UpdateUser where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 6}

instance FromJSON ReadUser where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 4 }
instance FromJSON WriteUser where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 5 }
instance FromJSON UpdateUser where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 6 }

--  The following instances are required for generating swagger stuff.
instance ToSchema User
instance ToSchema UserId
instance ToParamSchema UserId
instance ToSchema ReadUser
instance ToSchema WriteUser
instance ToSchema UpdateUser



-- Utility functions
-- | Encrypt some data given as text, with the specified number of passes, retunring a bytestring.
encryptText :: (MonadIO m) => Text -> Int -> m ByteString
encryptText txt = let bs = encodeUtf8 txt in return $ liftIO $ makePassword bs 20
