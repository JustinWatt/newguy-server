{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Login where

import           Control.Monad.Reader
import           Data.Int                         (Int64)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import           Data.Text.Encoding               (encodeUtf8)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                  selectFirst, selectList, (==.))

import           Data.Aeson                       (FromJSON, ToJSON, (.=), toJSON, object,
                                                  (.:), parseJSON, Value(Object))

import           GHC.Generics (Generic)

import           Network.Wai                      (Application)
import           Servant

import           Config                           (App (..), Config (..))
import           Models
import           Auth
import           Crypto.PasswordStore


data Login =
  Login
  { email     :: T.Text
  , password  :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON Login where

loginServer :: ServerT LoginAPI App
loginServer = login

type LoginAPI =
  ReqBody '[JSON] Login :> Get '[JSON] Credentials

login :: Login -> App Credentials
login (Login e pw) = do
    maybeUser <- runDb (selectFirst [UserEmail ==. e] [])

    case maybeUser of
      Nothing ->
        throwError err403

      Just (Entity uID (User _ _ phash)) ->
        if verifyPassword (encodeUtf8 pw) (encodeUtf8 phash) then do
          secret <- asks getSecret
          return $ createToken secret uID
        else
         throwError err403


