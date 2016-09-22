{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Animal where

import           Config                           (App (..))
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class           (liftIO, MonadIO)

import Data.Aeson
import Data.Aeson.TH

import Data.Int (Int64)
import Data.Maybe

import Api.Organization (isOrgMember)
import Models
import Auth (Claims(..))

import GHC.Generics (Generic)
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import OrganizationRole 

import Servant

type AnimalAPI = "animals" :> (
       AuthProtect "jwt" :> Capture "animalID" AnimalId :> Get '[JSON] (Entity Animal)
  :<|> AuthProtect "jwt" :> ReqBody '[JSON] Animal :> PostCreated '[JSON] Int64
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


createAnimal :: Claims -> Animal -> App Int64
createAnimal (Claims uID) animal@Animal{..} = do
   runDb $ do
    maybeOrgMember <- isOrgMember Member uID animalOrganizationId

    case maybeOrgMember of
      Nothing ->
        return $ Left err403
      Just _ -> do
        newAnimal <- P.insertBy animal

        case newAnimal of
          Left err ->
            return $ Left err403
          Right a ->
            return $ Right $ fromSqlKey a

  case result of
    Left err ->
      throwError err
    Right a ->
      return a
