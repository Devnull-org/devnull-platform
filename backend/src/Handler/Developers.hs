module Handler.Developers
  ( developersHandler
  ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Text     (Text)
import           Database.User (UserResponse, developersSelect, getAllUsers,
                                mkUserResponse)
import           Prelude
import           Types         (AppM)

developersHandler :: AppM [UserResponse]
developersHandler = do
  users <- getAllUsers developersSelect
  return $ mkUserResponse <$> users
