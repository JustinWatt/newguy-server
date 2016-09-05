{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Auth where

import           Models
import           GHC.Generics (Generic)

import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.ByteString as BS

import           Data.Aeson           (FromJSON, ToJSON, (.=), toJSON, object,
                                       (.:), parseJSON, Value(Object))
import           Web.JWT
import           Data.Map as M
import           Database.Persist.Postgresql      (fromSqlKey)

data Credentials =
  Credentials
  { token :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Credentials where

data Claims =
  Claims
  { userID :: UserId
  }
  deriving (Eq, Show, Generic)


createToken :: Secret -> UserId -> Credentials
createToken s uID =
  let claims = def { unregisteredClaims = M.singleton "userID" . toJSON $ fromSqlKey uID }
  in Credentials $ encodeSigned HS256 s claims