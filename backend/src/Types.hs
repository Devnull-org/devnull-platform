{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( AppM
  , Env (..)
  ) where

import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT)
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           Servant                    (Handler)

data Env = Env { envConnectionString :: Connection}

type AppM = ReaderT Env Handler

