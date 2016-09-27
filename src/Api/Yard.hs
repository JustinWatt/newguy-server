{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Yard where

import           Config                           (App (..))
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class           (liftIO, MonadIO)
import           Control.Monad.Except

import           Data.Int (Int64)
import           Data.Maybe
import           Data.Time

import           Api.Organization
import           Models
import           Auth (Claims(..))

import           GHC.Generics (Generic)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import           OrganizationRole

import Servant

type YardAPI = "yards" :> (
       AuthProtect "jwt" :> Capture "yardID" YardId :> Get '[JSON] (Entity Yard)
  :<|> AuthProtect "jwt" :> ReqBody '[JSON] NewYard :> PostCreated '[JSON] Int64
       )

yardServer :: ServerT YardAPI App
yardServer =
       getYardByID
  :<|> createYard

getYardByID :: Claims -> YardId -> App (Entity Yard)
getYardByID (Claims uID) aID = do
  result <- runDb $
    select $
    from $ \ (yard, orgUser) -> do
    where_ $ orgUser ^. OrganizationUserUserId ==. val uID
            &&. yard ^. YardOrganizationId ==. orgUser ^. OrganizationUserOrganizationId
            &&. yard ^. YardId ==. val aID
    return yard

  case listToMaybe result of
    Nothing ->
      throwError err404
    Just yard ->
      return yard

mkYard :: NewYard -> UTCTime -> Yard
mkYard NewYard{..} t =
  Yard name organizationID t Nothing

createYard :: Claims -> NewYard -> App Int64
createYard (Claims uID) ny@NewYard{..} = do
  member <- isOrgMember Admin uID organizationID

  if member then do
    now <- liftIO getCurrentTime
    newYard <- runDb $ P.insert $ mkYard ny now
    return $ fromSqlKey newYard
  else
    throwError err403
