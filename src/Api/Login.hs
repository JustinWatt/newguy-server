{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Login where

import           Control.Monad.Reader
import           Data.Int (Int64)
import           Data.Text
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                  selectFirst, selectList, (==.))

import           Network.Wai                      (Application)
import           Servant

import           Config                           (App (..), Config (..))
import           Models
import           Auth

loginServer :: ServerT LoginAPI App
loginServer = login

type LoginAPI =
  ReqBody '[JSON] Login :> Get '[JSON] Credentials

login :: Login -> App Credentials
login (Login (Auth.Email e) (Password p)) = do
    maybeUserID <- runDb (selectFirst [UserEmail ==. e
                                      ,UserPassword ==. p
                                      ] [])
    case maybeUserID of
      Nothing ->
        throwError err403
      Just (Entity uID _) -> do
        secret <- asks getSecret
        return $ createToken secret uID


