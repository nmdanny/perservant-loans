{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}


module Api.Person (personServer,PersonAPI) where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson                  (toJSON)
import           Data.Aeson.Encode           (encode)
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
                                              getBy, insert, selectFirst, selectList,
                                              toSqlKey, (==.))
import           GHC.Generics                (Generic)
import           Network.Wai                 (Application)

import           Data.Swagger                (ToParamSchema, ToSchema)
import           Servant
import           Servant.JS
import           Servant.Swagger

import           Api.Crud                    (CrudAPI, crudServer)
import           Config                      (App (..), Config (..), runDb)
import qualified Model
import           Model.Loan

import           Model.User

type PersonAPI =
         "users" :> Get '[JSON] [ReadUser]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] ReadUser
    :<|> "users" :> Capture "id" UserId :> Get '[JSON] ReadUser
    :<|> "users" :> ReqBody '[JSON] WriteUser :> Post '[JSON] UserId
    :<|> "users" :> ReqBody '[JSON] UpdateUser :> Put '[JSON] UserId
    :<|> "users" :> Capture "name" Text :> Capture "relation" LoanRelation :> Get '[JSON] [Loan]



personServer :: ServerT PersonAPI App
personServer = allPersons :<|> getPersonByName :<|> getPersonById :<|>  postPerson :<|> updateUser :<|> userLoans


allPersons :: App [ReadUser]
allPersons =  fmap readUser <$> runDb (selectList [] [])


getPersonByName :: Text -> App ReadUser
getPersonByName name = do
    maybeUser <- runDb $ getBy (Model.UniqueName name)
    maybe (throwError err404) (return . readUser) maybeUser

getPersonById :: UserId -> App ReadUser
getPersonById (UserId idInt) = do
  maybeUser <- runDb $ selectFirst [Model.UserId ==. toSqlKey idInt] []
  maybe (throwError err404) (return . readUser) maybeUser

postPerson :: WriteUser -> App UserId
postPerson user = (UserId . fromSqlKey) <$> doWriteUser user

updateUser :: UpdateUser -> App UserId
updateUser user = (UserId . fromSqlKey) <$> doUpdateUser user


-- | This function finds all loans where the given user is involved, depending on his relation to the loan
-- (being a borrower, a lender or either)
userLoans :: Text -> LoanRelation -> App [Loan]
userLoans name relation = do
  maybeUser <- runDb $ getBy (Model.UniqueName name)
  case maybeUser of
    Nothing -> throwError err404 { errBody = "No user with the given name exists."}
    Just user -> let loans = loansByUser' relation user
                 in fmap modelToLoan <$> loans



-- | A helper function for loansByUser.
-- TODO: find ways to make this function shorter, there's some repitition here.
loansByUser' :: LoanRelation -> Entity Model.User -> App [Model.Loan]
loansByUser' relation curUserEnt = do
  let curUserKey = entityKey curUserEnt
  let
      q :: E.SqlPersistT IO [Entity Model.Loan]
      q = case relation of
        Involved -> E.select
                  $ E.from $ \loan -> do
                    E.where_ $ loan ^. Model.LoanLender E.==. E.val curUserKey E.||. loan ^. Model.LoanBorrower E.==. E.val curUserKey
                    return loan
        _ ->
          let loanUserFk = if relation == Lender then Model.LoanLender else Model.LoanBorrower
          in                    E.select
                              $ E.from $ \loan -> do
                                E.where_ $ loan ^. loanUserFk E.==. E.val curUserKey
                                return loan
  loanEnts <- runDb q
  return $ entityVal <$> loanEnts




-- | The involvment of a user in a loan, he can either be the lender, the borrower
-- or either of them (involved)
data LoanRelation = Lender | Borrower | Involved
  deriving (Show,Eq,Generic,Typeable)

instance ToSchema LoanRelation
instance ToParamSchema LoanRelation

-- This allows us to capture a LoanRelation from a Url piece.
instance FromHttpApiData LoanRelation where
  parseUrlPiece txt = case T.toLower txt of
    "lending"   -> Right  Lender
    "borrowing" -> Right  Borrower
    "involved"  -> Right  Involved
    _           -> Left   "UrlPiece must be either lending, borrowing or involved."
