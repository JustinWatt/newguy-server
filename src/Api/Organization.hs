{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Api.Organization where

import           Data.Int                         (Int64)
import           Data.Maybe

import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class           (liftIO, MonadIO)
import           Control.Monad.Trans              (lift)
import           Control.Monad.Trans.Reader       (ReaderT)

import           OrganizationRole
import           Models
import qualified Database.Persist.Postgresql as P
import           Database.Esqueleto

import           Config                           (App (..))
import           Servant
import           Auth                             (Claims(..))
import           GHC.Generics         (Generic)

import           Data.Aeson
import           Data.Aeson.TH

type OrganizationAPI = "organizations" :> (
        AuthProtect "jwt" :> Get '[JSON] [Entity Organization]
   :<|> AuthProtect "jwt" :> ReqBody '[JSON] Organization :> PostCreated '[JSON] Int64
   :<|> AuthProtect "jwt" :> Capture "organizationId" OrganizationId :> "animals" :> Get '[JSON] [Entity Animal]
  -- :<|> AuthProtect "jwt" :> Capture "organizationId" OrganizationId :> "users" :> Get '[JSON] [Entity User]
   :<|> AuthProtect "jwt" :> Capture "organizationId" OrganizationId :> ReqBody '[JSON] OrganizationInvitation :>  "users" :> PostNoContent '[JSON] NoContent
   )

organizationServer :: ServerT OrganizationAPI App
organizationServer =
       allOrganizations
  :<|> createOrganization
  :<|> organizationAnimals
  :<|> inviteUser

organizationMembers :: Claims -> OrganizationId -> App [Entity User]
organizationMembers (Claims uID) oID = undefined
  -- runDb $
  --   select $
  --   from $ \(user `InnerJoin` orgUser) -> do
  --   on  (user ^. UserId ==. orgUser ^. OrganizationUserUserId)
  --   where_ $ val uID ==. (orgUser ^. OrganizationUserOrganizationId)
  --   return user

organizationAnimals :: Claims -> OrganizationId -> App [Entity Animal]
organizationAnimals (Claims uID) oID =
  runDb $
    select $
    from $ \(animal, orgUser) -> do
    where_ $ val uID ==. orgUser ^. OrganizationUserUserId
             &&. val oID ==. orgUser ^. OrganizationUserOrganizationId
             &&. animal ^. AnimalOrganizationId ==. val oID
    return animal

allOrganizations :: Claims -> App [Entity Organization]
allOrganizations (Claims uID) =
  runDb $
    select $
    from $ \(org, orgUser) -> do
    where_ $ org ^. OrganizationId ==. orgUser ^. OrganizationUserOrganizationId
             &&. orgUser ^. OrganizationUserUserId ==. val uID
    return org

createOrganization :: Claims -> Organization -> App Int64
createOrganization (Claims uID) org = runDb $ do
  newOrg <- insert $ Organization (organizationName org)
  insert $ OrganizationUser uID newOrg Admin True
  return $ fromSqlKey newOrg

isOrgMember :: MonadIO m => OrganizationRole -> UserId -> OrganizationId -> ReaderT SqlBackend m (Maybe (Entity OrganizationUser))
isOrgMember oRole uID oID =
  P.selectFirst [ OrganizationUserUserId P.==. uID
                , OrganizationUserOrganizationId P.==. oID
                , OrganizationUserRole P.>=. oRole] []

isOrgAdmin :: MonadIO m => UserId -> OrganizationId -> ReaderT SqlBackend m (Maybe (Entity OrganizationUser))
isOrgAdmin = isOrgMember Admin

mkOrganizationUser :: OrganizationInvitation -> OrganizationUser
mkOrganizationUser (OrganizationInvitation uID oID role) =
  OrganizationUser uID oID role False

inviteUser :: Claims -> OrganizationId -> OrganizationInvitation -> App NoContent
inviteUser (Claims uID) oID invite = do
  result <- runDb $ do
    maybeOrgAdmin <- isOrgAdmin uID oID

    case maybeOrgAdmin of
      Nothing ->
        return $ Left $ err403
      Just _ -> do
        orgInvite <- P.insertBy $ mkOrganizationUser invite

        case orgInvite of
          Left err ->
            return $ Left err403
          Right _ ->
            return $ Right NoContent

  case result of
    Left err ->
      throwError err
    Right _ ->
      return NoContent

data OrganizationInvitation =
  OrganizationInvitation
  { userID         :: UserId
  , organizationID :: OrganizationId
  , role           :: OrganizationRole
  }
  deriving (Eq, Show, Generic)

instance FromJSON OrganizationInvitation where

instance ToJSON OrganizationInvitation where
