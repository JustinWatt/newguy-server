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
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans              (lift)

import           OrganizationRole
import           Models
import qualified Database.Persist.Postgresql as P
import           Database.Esqueleto

import           Config                           (App (..), Config (..))
import           Servant
import           Auth                             (Claims(..))
import           GHC.Generics         (Generic)

import           Data.Aeson

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
    from $ \(animal `InnerJoin` orgUser) -> do
    on  (animal ^. AnimalOrganizationId ==. orgUser ^. OrganizationUserOrganizationId)
    where_ $ val uID ==. (orgUser ^. OrganizationUserUserId)
    where_ $ val oID ==. (orgUser ^. OrganizationUserOrganizationId)
    return animal

allOrganizations :: Claims -> App [Entity Organization]
allOrganizations (Claims uID) =
  runDb $
    select $
    from $ \(org `InnerJoin` orgUser) -> do
    on (org ^. OrganizationId ==. orgUser ^. OrganizationUserOrganizationId)
    where_ $ val uID ==. (orgUser ^. OrganizationUserUserId)
    return org

createOrganization :: Claims -> Organization -> App Int64
createOrganization (Claims uID) org = runDb $ do
  newOrg <- insert $ Organization (organizationName org)
  insert $ OrganizationUser uID newOrg Admin True
  return $ fromSqlKey newOrg

isOrgMember oRole uID oID =
  (P.selectFirst [ OrganizationUserUserId P.==. uID
                 , OrganizationUserOrganizationId P.==. oID
                 , OrganizationUserRole P.>=. oRole]  [])

isOrgAdmin = isOrgMember Admin

inviteUser :: Claims -> OrganizationId -> OrganizationInvitation -> App NoContent
inviteUser (Claims uID) oID OrganizationInvitation{..} = do
  maybeOrgAdmin <- runDb $ isOrgAdmin uID oID

  case maybeOrgAdmin of
    Nothing ->
      throwError err403
    Just _ -> do
      invite <- runDb $ P.insertBy $ OrganizationUser userID organizationId role False

      case invite of
        Left err ->
          throwError err404 { errBody = "User does not exist" }
        Right _ ->
          return NoContent

data OrganizationInvitation =
  OrganizationInvitation
  { userID         :: UserId
  , organizationId :: OrganizationId
  , role           :: OrganizationRole
  }
  deriving (Eq, Show, Generic)

instance FromJSON OrganizationInvitation where


