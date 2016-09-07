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

type OrganizationAPI =
        "organizations" :> AuthProtect "jwt" :> Get '[JSON] [Entity Organization]
  :<|>  "organizations" :> AuthProtect "jwt" :> ReqBody '[JSON] Organization :> Post '[JSON] Int64

organizationServer :: ServerT OrganizationAPI App
organizationServer =
       allOrganizations
  :<|> createOrganization

allOrganizations :: Claims -> App [Entity Organization]
allOrganizations (Claims uID) = do
  orgs <- runDb $
    E.select $
    E.from $ \(org `E.InnerJoin` orgUser) -> do
    E.on (org E.^. OrganizationId E.==. orgUser E.^. OrganizationUserOrganizationId)
    E.where_ $ E.val uID E.==. (orgUser E.^. OrganizationUserUserId)
    return org
  return orgs

createOrganization :: Claims -> Organization -> App Int64
createOrganization c org = runDb $ do
  newOrg <- insert $ Organization (organizationName org)
  insert $ OrganizationUser (userID c) newOrg Admin
  return $ fromSqlKey newOrg
