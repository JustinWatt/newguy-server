{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Api.User where

import           Control.Monad.Except
import           Control.Monad.Reader.Class

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.TH

import           Data.String.Conversions          (cs)
import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Internal as BS

import           Data.Int                         (Int64)

import           Data.Text                        (Text, strip)
import           Data.Char                        (toLower)

import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                  selectFirst, selectList, (==.))
import           Network.Wai                      (Application)
import           Servant
import qualified Database.Esqueleto as E

import           Config                           (App (..), Config (..))
import           Models
import           Auth

import           Text.Email.Validate
import           Crypto.PasswordStore

type UserAPI =
       "signup" :> ReqBody '[JSON] Registration :> PostCreated '[JSON] Int64
  :<|> "login"  :> ReqBody '[JSON] Login :> Post '[JSON] Credentials

-- | The server that runs the UserAPI
userServer :: ServerT UserAPI App
userServer =
       registerUser
  :<|> login

registerUser :: Registration -> App Int64
registerUser reg = do

  registeredUser <- liftIO $ registrationToUser reg

  case registeredUser of
    Left registrationError ->
      throwError $ err400 { errBody = cs $ show registrationError }
    Right user -> do
      newUser <- runDb $ E.insertBy user
      case newUser of
        Left _ ->
          throwError err400 { errBody = "Email in Use" :: ByteString}
        Right key ->
          return $ fromSqlKey key

-- | Returns a user by email or throws a 404 error.
singleUser :: Text -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserEmail ==. (str :: Text)] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

login :: Login -> App Credentials
login (Login e pw) = do
    maybeUser <- runDb (selectFirst [UserEmail ==. e] [])

    case maybeUser of
      Nothing ->
        throwError err404

      Just (Entity uID (User _ _ phash)) ->
        if verifyPassword (cs pw) (cs phash) then do
          secret <- asks getSecret
          return $ createToken secret uID
        else
         throwError err404

data RegistrationError =
    PasswordsDontMatch
  | InvalidEmailAddress ByteString String
  deriving (Eq)

instance Show RegistrationError where
  show PasswordsDontMatch =
    "Passwords do not match"

  show (InvalidEmailAddress attempt reason) =
    "Invalid email address: " ++ cs attempt

validatePassword :: Text -> Text -> Either RegistrationError Text
validatePassword pWord pWordConfirm =
  if pWord /= pWordConfirm then
    Left PasswordsDontMatch
  else
    Right pWord

validateEmail :: BS.ByteString -> Either RegistrationError Text
validateEmail bs =
  case validate bs of
    Left  errStr -> Left $ InvalidEmailAddress (cs bs) errStr
    Right email -> Right $ cs (toByteString email)

encryptPassword :: Text -> IO Text
encryptPassword plainPassword = do
  ep <- makePassword (cs plainPassword) 17
  return $ cs ep

validateRegistration :: Registration -> Either RegistrationError Registration
validateRegistration reg@Registration{..} = do
  emailE    <- validateEmail $ cs email
  passwordE <- validatePassword password passwordConfirm

  return reg

registrationToUser :: Registration -> IO (Either RegistrationError User)
registrationToUser reg =
  case validateRegistration reg of
    Left err ->
      return $ Left err
    Right Registration{..} -> do
      ep <- encryptPassword password
      return $ Right $ User Nothing email ep


-- allUserPosts :: UserId -> App [PublicPost]
-- allUserPosts uID = do
--   posts <- runDb $
--     E.select $
--     E.from $ \p -> do
--     E.where_ $ p E.^. PostAuthorId E.==. E.val uID
--     return p
--   return $ map (\(Entity i p) -> postToPublicPost i p) posts

--createUserPost :: UserId -> Models.Post -> App Int64
-- createUserPost uID p =
--   if uID /= postAuthorId p
--     then
--       throwError err409
--     else do
--       newPost <- runDb $ insert $ Models.Post (postTitle p) (postBody p) (postAuthorId p)
--       return $ fromSqlKey newPost


