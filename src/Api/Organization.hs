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
import           Data.String.Conversions          (cs)

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

type OrganizationAPI =
        "organizations" :> AuthProtect "jwt" :> Get '[JSON] [Entity Organization]
   :<|> "organizations" :> AuthProtect "jwt" :> ReqBody '[JSON] Organization :> PostCreated '[JSON] Int64
   :<|> "organization"  :> AuthProtect "jwt" :> Capture "orgId" OrganizationId :> ReqBody '[JSON] OrganizationInvitation :> "users" :> PostNoContent '[JSON] NoContent
   :<|> "organization"  :> AuthProtect "jwt" :> Capture "orgId" OrganizationId :> "animals" :> Get '[JSON] [Entity Animal]
   :<|> "organization"  :> AuthProtect "jwt" :> Capture "orgId" OrganizationId :> "yards" :> Get '[JSON] [Entity Yard]
  -- :<|> AuthProtect "jwt" :> Capture "organizationId" OrganizationId :> "users" :> Get '[JSON] [Entity User]

organizationServer :: ServerT OrganizationAPI App
organizationServer =
       allOrganizations
  :<|> createOrganization
  :<|> inviteUser
  :<|> organizationAnimals
  :<|> organizationYards

organizationMembers :: Claims -> OrganizationId -> App [Entity User]
organizationMembers (Claims uID) oID = undefined
  -- runDb $
  --   select $
  --   from $ \(user `InnerJoin` orgUser) -> do
  --   on  (user ^. UserId ==. orgUser ^. OrganizationUserUserId)
  --   where_ $ val uID ==. (orgUser ^. OrganizationUserOrganizationId)
  --   return user

organizationYards :: Claims -> OrganizationId -> App [Entity Yard]
organizationYards (Claims uID) oID =
  runDb $
    select $
    from $ \(yard, orgUser) -> do
    where_ $ val uID ==. orgUser ^. OrganizationUserUserId
             &&. val oID ==. orgUser ^. OrganizationUserOrganizationId
             &&. yard ^. YardOrganizationId ==. val oID
    return yard

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

isOrgMember :: OrganizationRole -> UserId -> OrganizationId -> App Bool
isOrgMember oRole uID oID = do
  maybeOrgMember <- runDb $ P.selectFirst [OrganizationUserUserId         P.==. uID
                                          ,OrganizationUserOrganizationId P.==. oID
                                          ,OrganizationUserRole           P.<=. oRole
                                          ,OrganizationUserAccepted       P.==. True] []
  return $ isJust maybeOrgMember

isOrgAdmin :: UserId -> OrganizationId -> App Bool
isOrgAdmin = isOrgMember Admin

mkOrganizationUser :: OrganizationInvitation -> OrganizationUser
mkOrganizationUser (OrganizationInvitation uID oID role) =
  OrganizationUser uID oID role False

inviteUser :: Claims -> OrganizationId -> OrganizationInvitation -> App NoContent
inviteUser (Claims uID) oID invite = do
  admin <- isOrgAdmin uID oID

  if admin then do
    orgInvite <- runDb $ P.insertBy $ mkOrganizationUser invite

    case orgInvite of
      Left err ->
        throwError $ err403
      Right _ ->
        return NoContent
  else
      throwError $ err401

