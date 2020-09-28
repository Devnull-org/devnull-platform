{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.User where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader, ask)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Profunctor.Product         (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text
import           Database.PostgreSQL.Simple      (Connection)
import           Opaleye
import           Prelude
import           Types
import           Database.Role (UserRole (..), PGRole (..))

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
type UserField = User' (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText) (Field PGRole)

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
printSql = putStrLn . maybe "Empty select" id . showSql
