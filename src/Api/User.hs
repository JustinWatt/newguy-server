{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)
import qualified Database.Esqueleto as E

import           Config                      (App (..), Config (..))
import           Models

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Servant.Post '[JSON] Int64
    :<|> "user" :> Capture "id" UserId :> "posts" :> Get '[JSON] [Models.PublicPost]
    :<|> "user" :> Capture "id" UserId :> "posts" :> ReqBody '[JSON] Models.Post :> Servant.Post '[JSON] Int64

-- | The server that runs the UserAPI
userServer :: ServerT UserAPI App
userServer =
       allUsers
  :<|> singleUser
  :<|> createUser
  :<|> allUserPosts
  :<|> createUserPost

-- | Returns all users in the database.
allUsers :: App [Entity User]
allUsers =
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: String -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: User -> App Int64
createUser p = do
    newUser <- runDb (insert (User (userName p) (userEmail p)))
    return $ fromSqlKey newUser

allUserPosts :: UserId -> App [PublicPost]
allUserPosts uID = do
  posts <- runDb $ E.select $
                   E.from $ \p -> do
                   E.where_ $ p E.^. PostAuthorId E.==. E.val uID
                   return p

  return $ map (\(Entity i p) -> postToPublicPost i p) posts

createUserPost :: UserId -> Models.Post -> App Int64
createUserPost uID p =
  if uID /= postAuthorId p
    then
      throwError err409
    else do
      newPost <- runDb $ insert $ Models.Post (postTitle p) (postBody p) (postAuthorId p)
      return $ fromSqlKey newPost


-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"
