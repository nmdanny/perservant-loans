module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           qualified Api.Person as PersonAPI
import           qualified Api.Loan  as LoanAPI
import           Config                      (Config (..), Environment (..), lookupSetting,
                                              makePool, setLogger)
import           Models                      (doMigrations)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    PersonAPI.generateJSAndSwagger
    LoanAPI.generateJSAndSwagger
    run port $ logger $ app cfg
