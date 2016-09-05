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

data Registration =
  Registration
  { regName     :: Text
  , regEmail    :: Text
  , regPassword :: Text
  , regPasswordConfirm :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Registration where

data RegistrationError =
    PasswordsDontMatch
  | InvalidEmailAddress ByteString String
  | NameEmpty Text
  deriving (Eq, Show)

validatePassword :: Text -> Text -> Either RegistrationError Text
validatePassword pWord pWordConfirm =
  if pWord /= pWordConfirm then
    Left PasswordsDontMatch
  else
    Right pWord

validateEmail :: ByteString -> Either RegistrationError Text
validateEmail bs =
  case validate bs of
    Left  errStr -> Left (InvalidEmailAddress bs errStr)
    Right email -> Right (decodeUtf8 (toByteString email))

validateName :: Text -> Either RegistrationError Text
validateName n =
  if strip n /= "" then
    Right (strip n)
  else
    Left $ NameEmpty n

encryptPassword :: Text -> IO Text
encryptPassword plainPassword = do
  ec <- makePassword (encodeUtf8 plainPassword) 17
  return $ decodeUtf8 ec

validateRegistration :: Registration -> Either RegistrationError Registration
validateRegistration reg@Registration{..} = do
  emailE    <- validateEmail (encodeUtf8 regEmail)
  passwordE <- validatePassword regPassword regPasswordConfirm
  nameE     <- validateName regName

  return reg

registrationToUser :: Registration -> IO (Either RegistrationError User)
registrationToUser reg =
  case validateRegistration reg of
    Left err ->
      return $ Left err
    Right Registration{..} -> do
      ec <- encryptPassword regPassword
      return $ Right $ User regName regEmail ec

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
