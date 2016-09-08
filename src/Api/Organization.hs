{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Api.Organization where

import           Data.Int                         (Int64)

import           OrganizationRole
import           Models
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                  selectFirst, selectList, (==.))
import qualified Database.Esqueleto as E

import           Config                           (App (..), Config (..))
import           Servant
import           Auth                             (Claims(..))

type OrganizationAPI = "organizations" :> (
        AuthProtect "jwt" :> Get '[JSON] [Entity Organization]
   :<|> AuthProtect "jwt" :> ReqBody '[JSON] Organization :> PostCreated '[JSON] Int64
   :<|> AuthProtect "jwt" :> Capture "organizationId" OrganizationId :> "animals" :> Get '[JSON] [Entity Animal]
--   :<|> AuthProtect "jwt" :> Capture "organizationId" OrganizationId :> "users" :> Get '[JSON] [Entity User]
   )

organizationServer :: ServerT OrganizationAPI App
organizationServer =
       allOrganizations
  :<|> createOrganization
  :<|> organizationAnimals

organizationMembers :: Claims -> OrganizationId -> App [Entity User]
organizationMembers (Claims uID) oID = undefined
  -- runDb $
  --   E.select $
  --   E.from $ \(user `E.InnerJoin` orgUser) -> do
  --   E.on  (user E.^. UserId E.==. orgUser E.^. OrganizationUserUserId)
  --   E.where_ $ E.val uID E.==. (orgUser E.^. OrganizationUserOrganizationId)
  --   return user

organizationAnimals :: Claims -> OrganizationId -> App [Entity Animal]
organizationAnimals (Claims uID) oID =
  runDb $
    E.select $
    E.from $ \(animal `E.InnerJoin` orgUser) -> do
    E.on  (animal E.^. AnimalOrganizationId E.==. orgUser E.^. OrganizationUserOrganizationId)
    E.where_ $ E.val uID E.==. (orgUser E.^. OrganizationUserUserId)
    E.where_ $ E.val oID E.==. (orgUser E.^. OrganizationUserOrganizationId)
    return animal

allOrganizations :: Claims -> App [Entity Organization]
allOrganizations (Claims uID) =
  runDb $
    E.select $
    E.from $ \(org `E.InnerJoin` orgUser) -> do
    E.on (org E.^. OrganizationId E.==. orgUser E.^. OrganizationUserOrganizationId)
    E.where_ $ E.val uID E.==. (orgUser E.^. OrganizationUserUserId)
    return org

createOrganization :: Claims -> Organization -> App Int64
createOrganization (Claims uID) org = runDb $ do
  newOrg <- insert $ Organization (organizationName org)
  insert $ OrganizationUser uID newOrg Admin
  return $ fromSqlKey newOrg
