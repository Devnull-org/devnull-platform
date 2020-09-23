module Main where

import Components.Home (homeComponent)
import Effect (Effect)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import Env (LogLevel (..))
import Halogen (liftEffect)
import Data.Maybe (Maybe(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude
import Request (BaseURL (..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    baseUrl = BaseURL "https://localhost:1234"
    logLevel = Dev

  -- Next, we'll maintain a global mutable reference which contains the profile for the currently
  -- authenticated user (if there is one). To start, we'll fill the mutable reference with `Nothing`
  -- since we don't yet have the user's profile.
  currentUser <- liftEffect $ Ref.new Nothing

  -- We'll also create a new bus to broadcast updates when the value of the current user changes;
  -- that allows all subscribed components to stay in sync about this value.
  userBus <- liftEffect Bus.make
  runUI homeComponent unit body
