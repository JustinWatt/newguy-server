{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Auth where

import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Config                           (App (..), Config (..))

import qualified Data.Map as M
import           Data.String.Conversions          (cs)
import           Models
import           GHC.Generics                     (Generic)

import           Data.Int                         (Int64)
import           Data.Text                        (Text, splitOn)
import           Data.Text.Encoding               (decodeUtf8)
import qualified Data.ByteString as BS

import           Data.Aeson                       (FromJSON, ToJSON, (.=), toJSON, object,
                                                  (.:), parseJSON, Value(Object), fromJSON,
                                                  Result(..))
import           Web.JWT
import           Database.Persist.Postgresql      (fromSqlKey, toSqlKey)


import           Network.Wai                      (Request, requestHeaders)
import           Servant                          (throwError)
import           Servant.Server                   (Context ((:.), EmptyContext)
                                                  , err401, err403, errBody, Handler)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData
                                                  , mkAuthHandler)
import           Servant.API.Experimental.Auth    (AuthProtect)

data Credentials =
  Credentials
  { token :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Credentials where
instance FromJSON Credentials where

data Claims =
  Claims
  { userID :: UserId
  }
  deriving (Eq, Show, Generic)

createToken :: Secret -> UserId -> Credentials
createToken s uID =
  let claims = def { unregisteredClaims = M.singleton "userID" . toJSON $ fromSqlKey uID }
  in Credentials $ encodeSigned HS256 s claims

extractJWTClaims :: JWTClaimsSet -> Maybe Claims
extractJWTClaims jwtClaims =
  case M.lookup "userID" (unregisteredClaims jwtClaims) of
    Nothing ->
      Nothing
    Just userID ->
      case fromJSON userID of
        Error _ -> Nothing
        Success uID ->
          Just $ Claims $ toSqlKey uID

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

decodeAuthHeader :: Secret -> BS.ByteString -> Maybe (JWT VerifiedJWT)
decodeAuthHeader secret authHeader =
  authHeader
  |> cs
  |> splitOn " "
  |> Prelude.drop 1
  |> Prelude.head
  |> Web.JWT.decodeAndVerifySignature secret


decodeJWT :: Secret -> BS.ByteString -> Handler Claims
decodeJWT secret authHeader  =
  case decodeAuthHeader secret authHeader of
    Nothing ->
      throwError $ err403 { errBody = "Malformed Token" }
    Just token ->
      case extractJWTClaims (claims token) of
        Nothing ->
          throwError $ err403 { errBody = "Malformed Token" }
        Just claims' ->
          return claims'

authHandler :: Config -> AuthHandler Request Claims
authHandler config =
  let handler req =
        case Prelude.lookup "Authorization" (requestHeaders req) of
            Nothing ->
              throwError $ err401 { errBody = "Missing auth header" }
            Just authHeader ->
              decodeJWT (getSecret config) authHeader
  in mkAuthHandler handler

type instance AuthServerData (AuthProtect "jwt") = Claims

genAuthContext :: Config -> Context (AuthHandler Request Claims ': '[])
genAuthContext config = authHandler config :. EmptyContext
