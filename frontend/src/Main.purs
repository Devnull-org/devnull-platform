module Main where

import AppM (runAppM)
import Components.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import Env (LogLevel (..), Env, UserEnv)
import Halogen (liftEffect, Component, hoist, tell)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Prelude
import Request (BaseURL (..))
import Route (routeCodec)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    baseUrl = BaseURL "https://localhost:8080"
    logLevel = Dev

  userBus <- liftEffect Bus.make
  -- Next, we'll maintain a global mutable reference which contains the profile for the currently
  -- authenticated user (if there is one). To start, we'll fill the mutable reference with `Nothing`
  -- since we don't yet have the user's profile.
  currentUser <- liftEffect $ Ref.new Nothing
  let
    environment :: Env
    environment = { baseUrl, logLevel, userEnv }
      where
        userEnv :: UserEnv
        userEnv = { currentUser, userBus }
    rootComponent :: Component HH.HTML Router.Query {} Void Aff
    rootComponent = hoist (runAppM environment) Router.component
  -- Now we have the two things we need to run a Halogen application: a reference to an HTML element
  -- and the component to run there.
  --
  -- To run a Halogen application, use the `runUI` function. It accepts the component to run, arguments
  -- to provide to the component (in our case, the landing page), and the reference to an HTML element.
  -- It will start the Halogen application and return a record with two fields:
  --
  -- `query`, which lets us send queries down to the root component
  -- `subscribe`, which lets us listen and react to messages output by the root component
  --
  -- Note: Since our root component is our router, the "queries" and "messages" above refer to the
  -- `Query` and `Message` types defined in the `Conduit.Router` module. Only those queries and
  -- messages can be used, or else you'll get a compiler error.  rootComponent = hoist (runAppM environment) Router.component
  halogenIO <- runUI rootComponent {} body
  -- We're using hash-based routing, so we'll use the `matchesWith` function from `Routing.Hash` to
  -- listen for changes in the hash and parse the result (using our routing codec, `routeCodec`,
  -- along with the `parse` function from `Routing.Duplex`). Any time we parse a new location we'll
  -- trigger a `Navigate` query in the router.
  --
  -- If you feel confused by what's going on here, I'd recommend the `purescript-routing` and
  -- `purescript-routing-duplex` guides:
  --
  -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
  -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ tell $ Router.Navigate new

  pure unit
