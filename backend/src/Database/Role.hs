{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Role
  ( UserRole (..)
  -- , PGRole (..)
  , selectRoles
  , roleQuery
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           GHC.Generics
import           Opaleye
import           Prelude

data UserRole = RoleAdmin | RoleDeveloper | RoleUser  deriving (Show, Eq, Read, Enum)

$(deriveJSON defaultOptions ''UserRole)

-- data PGRole = PGRole { unRole :: PGText }

instance QueryRunnerColumnDefault PGText UserRole where
  queryRunnerColumnDefault = fmap (read . T.unpack) fieldQueryRunnerColumn

adaptRole :: Profunctor p => p Text (Field PGText) -> p UserRole (Field PGText)
adaptRole = dimap (T.pack . show) (unsafeCoerceColumn :: Field PGText -> Field PGText)

instance Default Constant UserRole (Column PGText) where
  def = Constant def'
    where
      def' :: UserRole -> (Column PGText)
      def' RoleAdmin     = pgStrictText "Admin"
      def' RoleDeveloper = pgStrictText "Developer"
      def' RoleUser      = pgStrictText "User"

data Roles' a b c = Roles
  { rolesAdmin     :: a
  , rolesDeveloper :: b
  , rolesUser      :: c
  } deriving (Generic, Show)

type Roles = Roles' UserRole UserRole UserRole
type RolesColumns = Roles' (Field PGText) (Field PGText) (Field PGText)

$(makeAdaptorAndInstance "pRoleSelect" ''Roles')

selectRoles :: Table RolesColumns RolesColumns
selectRoles = Table "role" $ pRoleSelect Roles
  { rolesAdmin = required "RoleAdmin"
  , rolesDeveloper = required "RoleDeveloper"
  , rolesUser = required "RoleUser"
  }

roleQuery :: Query RolesColumns
roleQuery = queryTable selectRoles
