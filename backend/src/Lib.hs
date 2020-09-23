{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Tables

data Env =
  Env
    { envConnectionString :: String
    } deriving Show

type API = "users" :> Get '[JSON] [UserResponse]

startApp :: IO ()
startApp = do
  let env = Env "connection-string"
  runReaderT (liftIO $ run 9009 app) env

app :: Application
app = simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [UserResponse]
users = undefined
