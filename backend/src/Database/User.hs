{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.User
  ( User (..)
  , UserResponse (..)
  , mkUserResponse
  , userSelect
  , developersSelect
  , getAllUsers
  , selectAllUsers
  ) where

import           Control.Arrow                   (returnA)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader, ask)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe                      (fromMaybe)
import           Data.Profunctor.Product         (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text
import           Database.PostgreSQL.Simple      (Connection)
import           Database.Role                   (UserRole (..))
import           Debug.Trace
import           Opaleye
import           Prelude
import           Types

data User' a b c d e f =
  User
    { userName      :: a
    , userEmail     :: b
    , userFirstName :: c
    , userLastName  :: d
    , userPassword  :: e
    , userRole      :: f
    } deriving Show

data UserResponse =
  UserResponse
    { userResponseUserName      :: Text
    , userResponseUserEmail     :: Text
    , userResponseUserFirstName :: Text
    , userResponseUserLastName  :: Text
    , userResponseUserRole      :: UserRole
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserResponse)

type User = User' Text Text Text Text Text UserRole
type UserField = User' (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserField UserField
userTable =
  table "user"
   (pUser
      User
        { userName = tableField "username"
        , userEmail = tableField "email"
        , userFirstName = tableField "firstname"
        , userLastName = tableField "lastname"
        , userPassword = tableField "password"
        , userRole = tableField "role"
        }
   )

userSelect :: Select UserField
userSelect = selectTable userTable

developersSelect :: Select UserField
developersSelect = do
  u <- selectTable userTable
  viaLateral restrict $ userRole u .== toFields RoleDeveloper
  return u

getAllUsers
  :: (MonadIO m, MonadReader Env m)
  => Select UserField
  -> m [User]
getAllUsers s = do
  Env {..} <- ask
  liftIO $ runSelect envConnectionString s

mkUserResponse :: User -> UserResponse
mkUserResponse User {..} =
  UserResponse
    { userResponseUserName      = userName
    , userResponseUserEmail     = userEmail
    , userResponseUserFirstName = userFirstName
    , userResponseUserLastName  = userLastName
    , userResponseUserRole      = userRole
    }

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty select" . showSql

selectAllUsers :: Connection -> IO [User]
selectAllUsers conn = runSelect conn developersSelect
