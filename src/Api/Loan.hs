{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Loan (loanServer,LoanAPI)where

-- General/built in haskell modules
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString(ByteString)
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)


--  Database and Servant modules
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, toSqlKey, insert, delete, update, replace, get, Key, entityKey, entityVal,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.Server (BasicAuthCheck(..))
import           Servant.API.Experimental.Auth (AuthProtect)
import           Servant.Server.Experimental.Auth (AuthHandler,AuthServerData,mkAuthHandler)
import           Servant.JS

-- Project's modules
import           Config                      (Config (..), App(..), runDb)
import qualified Model
import           Model.Loan
import           Api.Crud (crudServer, CrudAPI)
import qualified Api.Crud as Crud

type LoanAPI =
         "loans" :> Get '[JSON] [(LoanId,Loan)]
    :<|> "loans" :> ReqBody '[JSON] Loan :> Post '[JSON] LoanId
{-
    :<|> "loans" :> ReqBody '[JSON] Loan :> Post '[JSON] Int64
--  :<|>   BasicAuth "involved-realm" User :> "loans" :>  Capture "id" (Key Loan) :> Get '[JSON] (Entity Loan) -- get
    :<|> "loans" :>  Capture "id" LoanId :> Get '[JSON] Loan -- get

    :<|> "loans" :>  Capture "id" LoanId :> DeleteNoContent '[JSON] NoContent -- delete
    :<|> "loans" :>  Capture "id" LoanId :> ReqBody '[JSON] Loan :> PutNoContent '[JSON] NoContent -- put
-}

{-
-- | Checks that the given BasicAuth credentials correspond to a user
-- TODO: Make sure that encoding is correct, since BasicAuth uses bytestrings, whereas my model uses text
-- TODO: Implement encryption.
-- TODO: Find a way to get a 'Config' into the IO monad.
userAuthCheck :: BasicAuthCheck User
userAuthCheck = BasicAuthCheck check where
  check (BasicAuthData username userpass) = do
    dbEnt <- runDb $ selectFirst [UserName ==. decodeUtf8 username] []
    case entityVal <$> dbEnt of
      Nothing -> return NoSuchUser
      Just user -> if userPassword user == decodeUtf8 userpass then
        return $ Authorized user
        else return BadPassword

-}

loanServer :: ServerT LoanAPI App
loanServer =  allLoans
         :<|> postLoan
{-
         :<|> Crud.crudPost
         :<|> Crud.crudGet
         :<|> Crud.crudDelete
         :<|> Crud.crudPut
-}

allLoans :: App [(LoanId,Loan)]
allLoans = fmap (\(Entity k model) -> (LoanId $ fromSqlKey k,modelToLoan model)) <$> runDb (selectList [] [])

postLoan :: Loan -> App LoanId
postLoan = fmap (LoanId . fromSqlKey) . runDb . insert . loanToModel

getLoan :: LoanId -> App Loan
getLoan (LoanId loanId) = do
  maybeLoan <- runDb $ selectFirst [Model.LoanId ==. toSqlKey loanId] []
  maybe (throwError err404) (return . modelToLoan .  entityVal ) maybeLoan
