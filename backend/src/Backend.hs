{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Backend
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import           Data.Text                   (Text)
import           Database.PostgreSQL.Simple  (Connection, connectPostgreSQL)
import           Database.User               (UserResponse (..), getAllUsers,
                                              mkUserResponse, userSelect)
import           Handler.Developers          (developersHandler)
import           Handler.Home                (HomeResponse (..), homeHandler)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Types                       (AppM, Env (..))

type API =
      "home" :> Get '[JSON] HomeResponse :<|>
      "developers" :> Get '[JSON] [UserResponse]

startApp :: IO ()
startApp = do
  pgConnection <- connectPostgreSQL "host=localhost port=5432 dbname=devnull user=devnull connect_timeout=10"
  let env = Env pgConnection
  liftIO $ run 9009 (app env)

nt :: Env -> AppM a -> Handler a
nt env x = runReaderT x env

app :: Env -> Application
app env =
  simpleCors $
    serve api $ hoistServer api (nt env) server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = homeHandler :<|> developersHandler
