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

module Model where

import Data.ByteString (ByteString)
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
User
    name Text
    email Text
    password ByteString
    createDate UTCTime default=CURRENT_DATE
    updateDate UTCTime default=CURRENT_DATE
    UniqueEmail email
    UniqueName name
    deriving Show Generic Typeable

Loan json
   amount Int
   lender UserId
   borrower UserId
   startDate UTCTime default=CURRENT_DATE
   dueDate UTCTime
   finishedDate UTCTime Maybe
   createDate UTCTime default=CURRENT_DATE
   updateDate UTCTime default=CURRENT_DATE
   deriving Show Generic Typeable
|]



doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll
