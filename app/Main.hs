module Main where

import           Data.Text                   (pack)
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger, corsPolicy)
import           Models                      (doMigrations)
import           Safe                        (readMay)

import           Web.JWT                     (secret)


-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    jwtSecret <- lookupSetting "JWTSECRET" "secret"
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env, getSecret = secret $ pack jwtSecret }
        logger = setLogger env
        cors = corsPolicy env
    runSqlPool doMigrations pool
    putStrLn $ "Newguy API! Port: " ++ show port
    run port $ logger $ cors $ app cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
