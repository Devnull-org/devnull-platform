module Components.Developers
  ( component
  ) where

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (Either (..), hush)
import Data.List
import Data.Maybe (fromMaybe, Maybe (..))
import Data.String.NonEmpty (unsafeFromString)
import Debug.Trace
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Pathy.Name (Name(..))
import Pathy.Path (rootDir, (</>), dir', file')
import Pathy.Printer (printPath, posixPrinter)
import Pathy.Sandboxed (sandbox)
import Prelude
import Simple.JSON (readJSON)

type DevelopersResponse =
       { userResponseUserName             :: String
       , userResponseUserEmail            :: String
       , userResponseUserFirstName        :: String
       , userResponseUserLastName         :: String
       , userResponseUserRole             :: String
       }

type State =
    { loading :: Boolean
    , content :: Array DevelopersResponse
    }

data Action input
  = Initialize

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: initialState
    , render: ui
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

initialState :: forall i. i -> State
initialState _ =
  { loading : true
  , content : []
  }

renderDev :: DevelopersResponse -> _
renderDev devResponse =
  HH.div [HP.class_ (H.ClassName "row border m-4 p-4")]
    [ HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [ HH.p_ [HH.text "Username: "]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [HH.h5_ [HH.text devResponse.userResponseUserName]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [ HH.p_ [HH.text "First Name: "]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [HH.h5_ [HH.text devResponse.userResponseUserFirstName]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [ HH.p_ [HH.text "Last Name: "]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [HH.h5_ [HH.text devResponse.userResponseUserLastName]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [ HH.p_ [HH.text "Email: "]
             ]
    , HH.div [HP.class_ (H.ClassName "col-xs-6 w-50")]
             [HH.h5_ [HH.text devResponse.userResponseUserEmail]
             ]
    ]

ui :: forall m input. State -> H.ComponentHTML (Action input) () m
ui st = do
  let imageDir =
        rootDir </>
        (dir' $ Name (unsafePartial $ unsafeFromString "static")) </>
        (dir' $ Name (unsafePartial $ unsafeFromString "images"))
      imageFile = file' $ Name (unsafePartial $ unsafeFromString "devnull_logo.png")
      sandboxedPath = sandbox imageDir imageFile
      stringPath = fromMaybe "path-not-found" (printPath posixPrinter <$> sandboxedPath)
      devList = HH.div_ (map renderDev st.content)
  HH.div [ HP.class_ (H.ClassName "container")]
    [ HH.div
        [ HP.class_ (H.ClassName "jumbothron text-center")]
        [ HH.img
            [HP.src stringPath]
        ]
        , HH.div
          [ HP.class_ (H.ClassName "container bg-white p-5")]
          [ HH.h1_ [HH.text "Developers"]
          , HH.div_ [devList]
          ]
    ]

handleAction ∷ forall o input m. MonadAff m => Action input → H.HalogenM State (Action input) () o m Unit
handleAction action =
  case action of
    Initialize -> do
      res <- H.liftAff $ AX.get AXRF.string ("http://localhost:9009/developers")
      case res of
        Left err -> do
          log $ "GET /developers response failed to decode: " <> AX.printError err
        Right response -> do
          case readJSON response.body of
            Right (r :: Array DevelopersResponse) -> do
              H.modify_
                (\st ->
                  st { loading = false
                     , content = r
                     }
                )
            Left e -> do
              log $ "Can't parse JSON. " <> show e
