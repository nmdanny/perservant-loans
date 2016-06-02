{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Person (personServer,PersonAPI,generateJSAndSwagger) where

import           Data.Time (getCurrentTime)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, get, Key,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.Swagger
import           Servant.JS

import           Config                      (Config (..), App(..), runDb)
import           Models
import           Api.Crud (crudServer, CrudAPI)

type PersonAPI =
         "users" :> Get '[JSON] [UserGET]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] UserGET
    :<|> "users" :> ReqBody '[JSON] UserPOST :> Post '[JSON] Int64
    :<|> "users" :> Capture "name" Text :> Capture "relation" LoanRelation :> Get '[JSON] [Loan]


personServer :: ServerT PersonAPI App
personServer = allPersons :<|> singlePerson :<|> createPerson :<|> userLoans


allPersons :: App [UserGET]
allPersons = do
    users <- runDb (selectList [] [])
    let userDTOs = map getUserDTO users
    return userDTOs


singlePerson :: Text -> App UserGET
singlePerson uname = do
    maybeUser <- runDb (selectFirst [UserName ==. uname] [])
    case maybeUser of
         Nothing     -> throwError err404
         Just user -> return $ getUserDTO user


createPerson :: UserPOST -> App Int64
createPerson userDTO = do
    curTime <- liftIO  getCurrentTime
    let user = postUserDTO userDTO curTime
    fromSqlKey `fmap` runDb (insert user)

userLoans :: Text -> LoanRelation -> App [Loan]
userLoans name relation = do
  mbUser <- runDb (selectFirst [UserName ==. name] [])
  case mbUser of
    Nothing -> throwError err404
    Just user -> loansByUser' relation user




-- | The involvment of a user in a loan, he can either be the lender, the borrower
-- or either of them (involved)
data LoanRelation = Lender | Borrower | Involved
  deriving (Show,Eq)

-- This allows us to capture a LoanRelation from a Url piece.
instance FromHttpApiData LoanRelation where
  parseUrlPiece txt = case T.toLower txt of
    "lending"   -> Right  Lender
    "borrowing" -> Right  Borrower
    "involved"  -> Right  Involved
    _           -> Left   "UrlPiece must be either lending, borrowing or involved."

-- | This function finds all loans where the given user is involved, depending on his relation to the loan
-- (being a borrower, a lender or either)
-- TODO: find ways to make this function shorter, there's some repitition here.
loansByUser' :: LoanRelation -> Entity User -> App [Loan]
loansByUser' relation curUserEnt = do
  let curUserKey = entityKey curUserEnt
  let
      q :: E.SqlPersistT IO [Entity Loan]
      q = case relation of
        Involved -> E.select
                  $ E.from $ \loan -> do
                    E.where_ $ loan ^. LoanLender E.==. E.val curUserKey E.||. loan ^. LoanBorrower E.==. E.val curUserKey
                    return loan
        _ ->
          let loanUserFk = if relation == Lender then LoanLender else LoanBorrower
          in                    E.select
                              $ E.from $ \loan -> do
                                E.where_ $ loan ^. loanUserFk E.==. E.val curUserKey
                                return loan
  loanEnts <- runDb q
  return $ entityVal <$> loanEnts



generateJSAndSwagger :: IO ()
generateJSAndSwagger = do
  writeJSForAPI (Proxy :: Proxy PersonAPI) vanillaJS "./assets/personAPI.js"
