{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Role
  ( UserRole (..)
  , PGRole (..)
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

data PGRole = PGRole { unRole :: PGText }

instance QueryRunnerColumnDefault PGRole UserRole where
  queryRunnerColumnDefault = fmap (read . T.unpack) fieldQueryRunnerColumn

adaptRole :: Profunctor p => p Text (Column PGText) -> p UserRole (Column PGRole)
adaptRole = dimap (T.pack . show) (unsafeCoerceColumn :: Column PGText -> Column PGRole)

instance Default Constant UserRole (Column PGRole) where
  def = adaptRole def

data Roles' a b c = Roles
  { rolesAdmin     :: a
  , rolesDeveloper :: b
  , rolesUser      :: c
  } deriving (Generic, Show)

type Roles = Roles' UserRole UserRole UserRole
type RolesColumns = Roles' (Column PGRole) (Column PGRole) (Column PGRole)

$(makeAdaptorAndInstance "pRoleSelect" ''Roles')

selectRoles :: Table RolesColumns RolesColumns
selectRoles = Table "role" $ pRoleSelect Roles
  { rolesAdmin = required "RoleAdmin"
  , rolesDeveloper = required "RoleDeveloper"
  , rolesUser = required "RoleUser"
  }

roleQuery :: Query RolesColumns
roleQuery = queryTable selectRoles
