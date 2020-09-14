{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import           Common.Route
import           Obelisk.Backend
import           Obelisk.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve  ->
      serve $ \case
        (BackendRoute_Missing :/ ()) -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
