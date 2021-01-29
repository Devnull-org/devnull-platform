{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Role
  ( UserRole (..)
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

data UserRole
  = RoleAdmin
  | RoleDeveloper
  | RoleUser
  deriving (Show, Eq, Read, Enum)

$(deriveJSON defaultOptions ''UserRole)

instance QueryRunnerColumnDefault PGText UserRole where
  queryRunnerColumnDefault = fmap (read . T.unpack) fieldQueryRunnerColumn

adaptRole :: Profunctor p => p Text (Field PGText) -> p UserRole (Field PGText)
adaptRole = dimap (T.pack . show) (unsafeCoerceColumn :: Field PGText -> Field PGText)

instance Default Constant UserRole (Column PGText) where
  def = adaptRole def

