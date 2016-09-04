{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Api.User where

import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                  selectFirst, selectList, (==.))
import           Network.Wai                      (Application)
import           Servant

import           Servant.JS                       (vanillaJS, writeJSForAPI)
import qualified Database.Esqueleto as E

import           Config                           (App (..), Config (..))
import           Models
import           OrganizationRole as OR
import           Auth

type UserAPI =
         Get '[JSON] [Entity User]
    :<|> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> ReqBody '[JSON] User :> Servant.Post '[JSON] Int64

-- | The server that runs the UserAPI
userServer :: ServerT UserAPI App
userServer =
       allUsers
  :<|> singleUser
  :<|> createUser


-- | Returns all users in the database.
allUsers :: App [Entity User]
allUsers =
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: Text -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserName ==. (str :: Text)] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: User -> App Int64
createUser u = do
    newUser <- runDb $ insert u
    return $ fromSqlKey newUser

--
type OrganizationAPI = Get '[JSON] [Entity Organization]
  :<|> Capture "userId" UserId :> ReqBody '[JSON] Organization :> Servant.Post '[JSON] Int64

organizationServer :: ServerT OrganizationAPI App
organizationServer =
       allOrganizations
  :<|> createOrganization

allOrganizations :: App [Entity Organization]
allOrganizations =
  runDb (selectList [] [])

createOrganization :: UserId -> Organization -> App Int64
createOrganization uID org = runDb $ do
  newOrg <- insert $ Organization (organizationName org)
  insert $ OrganizationUser uID newOrg OR.Admin
  return $ fromSqlKey newOrg


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


-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"


