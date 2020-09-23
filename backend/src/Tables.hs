{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Tables where

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

data User' a b c d e =
  User
    { userName      :: a
    , userEmail     :: b
    , userFirstName :: c
    , userLastName  :: d
    , userRole      :: e
    }

data UserResponse =
  UserResponse
    { userResponseUserName      :: Text
    , userResponseUserEmail     :: Text
    , userResponseUserFirstName :: Text
    , userResponseUserLastName  :: Text
    , userResponseUserRole      :: Text
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserResponse)

type User = User' Text Text Text Text Text
type UserField = User' (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserField UserField
userTable =
  table "user"
   (pUser
      User
        { userName = tableField "name"
        , userEmail = tableField "email"
        , userFirstName = tableField "first_name"
        , userLastName = tableField "last_name"
        , userRole = tableField "role"
        }
   )

userSelect :: Select UserField
userSelect = selectTable userTable

getAllUsers
  :: (MonadIO m, MonadReader Connection m)
  => Select UserField
  -> m [User]
getAllUsers s = do
  conn <- ask
  liftIO $ runSelect conn s

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . maybe "Empty select" id . showSql
