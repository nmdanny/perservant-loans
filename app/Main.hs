module Main where

import           Data.Aeson                  (toJSON)
import           Data.Aeson.Encode           (encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Swagger
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           Servant
import           Servant.JS
import           Servant.Swagger             (toSwagger)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import qualified Api.Loan                    as LoanAPI
import qualified Api.Person                  as PersonAPI
import qualified Api as                      API
import           Config                      (Config (..), Environment (..),
                                              lookupSetting, makePool,
                                              setLogger)
import           Model                       (doMigrations)

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
    writeJSForAPI (Proxy :: Proxy PersonAPI.PersonAPI) vanillaJS "./assets/personAPI.js"
    writeJSForAPI (Proxy :: Proxy LoanAPI.LoanAPI) vanillaJS "./assets/personAPI.js"
    LBS.writeFile "./assets/swaggerPersonAPI.json" $ encode $ toJSON $ toSwagger (Proxy :: Proxy PersonAPI.PersonAPI)
    LBS.writeFile "./assets/swaggerLoanAPI.json" $ encode $ toJSON $ toSwagger (Proxy :: Proxy LoanAPI.LoanAPI)
    LBS.writeFile "./assets/swaggerAPI.json" $ encode $ toJSON $ toSwagger (Proxy :: Proxy API.AppAPI)
    putStrLn "Finished DB migrations, and generation of JS and Swagger."
    run port $ logger $ app cfg
