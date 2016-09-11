{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Control.Monad.Reader
import           Data.Aeson           (FromJSON, ToJSON, (.=), toJSON, object,
                                       (.:), parseJSON, Value(Object))
import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           GHC.Generics         (Generic)
import           Text.Email.Validate

import           Config
import           Data.Time
import           Data.Text            (Text, strip, pack)
import           Data.Text.Encoding   (encodeUtf8, decodeUtf8)
import           Data.ByteString      (ByteString)

import           OrganizationRole
import           Crypto.PasswordStore

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    email Text
    UniqueEmail email
    password Text
    deriving Show

Organization json
    name Text
    deriving Show

OrganizationUser
    userId UserId
    organizationId OrganizationId
    role  OrganizationRole
    accepted Bool

Animal json
    name Text
    organizationId OrganizationId
    yardId         YardId Maybe
    deriving Show

Yard json
    name Text
    organizationId OrganizationId
    deriving Show

YardEvent json
    animalId AnimalId
    yardId   YardId
    entryDTS UTCTime default=now()
    exitDTS  UTCTime Maybe
    deriving Show

AnimalRelationship json
    animalIdA AnimalId
    animalIdB AnimalId
    strength  Int
    deriving Show
|]

-- data PublicPost =
--   PublicPost
--   { id :: PostId
--   , title :: String
--   , body  :: String
--   } deriving (Eq, Show, Generic)

-- postToPublicPost :: PostId -> Post -> PublicPost
-- postToPublicPost pid Post{..} =
--   PublicPost { title = postTitle, body = postBody, id = pid }

-- instance ToJSON PublicPost where

-- instance FromJSON PublicPost where

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
