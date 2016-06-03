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

module Model.Loan where

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
import           Model.User                 (UserId (..), unUserId)


newtype LoanId = LoanId Int64 deriving (Show,Eq,Generic,Typeable,Num,Ord,Bounded)

unLoanId :: LoanId -> Int64
unLoanId (LoanId i) = i


data Loan = Loan {
  loanAmount       :: Int
, loanLender       :: UserId
, loanBorrower     :: UserId
, loanStartDate    :: UTCTime
, loanDueDate      :: UTCTime
, loanFinishedDate :: Maybe UTCTime
, loanCreateDate   :: UTCTime
, loanUpdateDate   :: UTCTime
} deriving (Show,Generic,Typeable)

-- | Converts a basic 'Loan' type to a model loan type.
loanToModel :: Loan -> Model.Loan
loanToModel Loan{..} = Model.Loan loanAmount (toSqlKey $ unUserId loanLender) (toSqlKey $ unUserId loanBorrower)
                                  loanStartDate loanDueDate loanFinishedDate
                                  loanCreateDate loanUpdateDate
-- | Converts a 'Loan' model type to a basic haskell type.
modelToLoan :: Model.Loan -> Loan
modelToLoan Model.Loan{..} = Loan loanAmount (UserId $ fromSqlKey loanLender) (UserId $ fromSqlKey loanBorrower)
                                  loanStartDate loanDueDate loanFinishedDate
                                  loanCreateDate loanUpdateDate

-- JSON instances
instance ToJSON Loan where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 4}
instance FromJSON Loan where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = drop 4}
instance ToJSON LoanId where
  toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True}
instance FromJSON LoanId where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True}

--  The following instances are required for generating swagger stuff.
instance ToSchema LoanId
instance ToParamSchema LoanId
instance ToSchema Loan
