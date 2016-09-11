-- @OrganizationRole.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OrganizationRole where

import Database.Persist.TH
import GHC.Generics         (Generic)
import Data.Aeson

data OrganizationRole = Member | Admin
  deriving (Show, Read, Eq, Generic)


derivePersistField "OrganizationRole"


instance FromJSON OrganizationRole where

