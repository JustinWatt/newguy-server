-- @OrganizationRole.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OrganizationRole where

import Database.Persist.TH

data OrganizationRole = Member | Admin
  deriving (Show, Read, Eq)
derivePersistField "OrganizationRole"
