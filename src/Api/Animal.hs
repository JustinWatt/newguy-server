{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Animal where

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

type AnimalAPI = "animals" :> (
       AuthProtect "jwt" :> Capture "animalID" AnimalId :> Get '[JSON] (Entity Animal)
  :<|> AuthProtect "jwt" :> ReqBody '[JSON] NewAnimal :> PostCreated '[JSON] Int64
       )

animalServer :: ServerT AnimalAPI App
animalServer =
       getAnimalByID
  :<|> createAnimal

getAnimalByID :: Claims -> AnimalId -> App (Entity Animal)
getAnimalByID (Claims uID) aID = do
  result <- runDb $
    select $
    from $ \ (animal, orgUser) -> do
    where_ $ orgUser ^. OrganizationUserUserId ==. val uID
            &&. animal ^. AnimalOrganizationId ==. orgUser ^. OrganizationUserOrganizationId
            &&. animal ^. AnimalId ==. val aID
    return animal

  case listToMaybe result of
    Nothing ->
      throwError err404
    Just animal ->
      return animal

mkAnimal :: NewAnimal -> UTCTime -> Animal
mkAnimal NewAnimal{..} t =
  Animal name organizationID Nothing t Nothing

createAnimal :: Claims -> NewAnimal -> App Int64
createAnimal (Claims uID) na@NewAnimal{..} = do
  member <- isOrgMember Member uID organizationID

  if member then do
    now <- liftIO getCurrentTime
    newAnimal <- runDb $ P.insert $ mkAnimal na now
    return $ fromSqlKey newAnimal
  else
    throwError err403
