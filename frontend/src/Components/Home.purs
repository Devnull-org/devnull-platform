module Components.Home
  ( component
  ) where

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (Either (..), hush)
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

type HomeResponse =
       { homeResponseTitle          :: String
       , homeResponseMainText       :: String
       , homeResponseAdditionalText :: String
       }

type State =
    { loading :: Boolean
    , title :: String
    , mainText :: String
    , additionalText :: String
    }

data Action = DownloadContent

data Message = ReceivedData String

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: initialState
    , render: ui
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just DownloadContent
            }
    }

initialState :: forall i. i -> State
initialState _ =
  { loading : true
  , title : "loading..."
  , mainText : "loading..."
  , additionalText : "loading..."
  }

ui :: forall m. State -> H.ComponentHTML Action () m
ui st = do
  let imageDir =
        rootDir </>
        (dir' $ Name (unsafePartial $ unsafeFromString "static")) </>
        (dir' $ Name (unsafePartial $ unsafeFromString "images"))
      imageFile = file' $ Name (unsafePartial $ unsafeFromString "devnull_logo.png")
      sandboxedPath = sandbox imageDir imageFile
      stringPath = fromMaybe "path-not-found" (printPath posixPrinter <$> sandboxedPath)
  HH.div [ HP.class_ (H.ClassName "container")]
    [ HH.div
        [ HP.class_ (H.ClassName "jumbothron text-center")]
        [ HH.img
            [HP.src stringPath]
        ]
        , HH.div
          [ HP.class_ (H.ClassName "container bg-white p-5")]
          [ HH.h1_
              [HH.text st.title]
          , HH.p
              [ HP.class_ (H.ClassName "row bg-white p-4 border")]
              [ HH.text st.mainText]
          , HH.p_ [ HH.text "Services we provide:"]
           , HH.ul [HP.class_ (H.ClassName "list-group")]
               [ HH.li [HP.class_ (H.ClassName "list-group-item")]
                   [HH.text "writing production level code"]
               , HH.li [HP.class_ (H.ClassName "list-group-item")]
                   [HH.text "code optimization"]
               , HH.li [HP.class_ (H.ClassName "list-group-item")]
                   [HH.text "debugging and fixing problems with execution speed and/or large memory consumption"]
               ]
           , HH.p
              [ HP.class_ (H.ClassName "bg-white p-4")]
              [ HH.text st.additionalText
              ]
          ]
    ]

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction action =
  case action of
    DownloadContent -> do
      res <- H.liftAff $ AX.get AXRF.string ("http://localhost:9009/home")
      case res of
        Left err -> do
          log $ "GET /home response failed to decode: " <> AX.printError err
        Right response -> do
          case readJSON response.body of
            Right (r :: HomeResponse) -> do
              H.modify_
                (\st ->
                  st { loading = false
                     , title = r.homeResponseTitle
                     , mainText = r.homeResponseMainText
                     , additionalText = r.homeResponseAdditionalText
                     }
                )
            Left e -> do
              log $ "Can't parse JSON. " <> show e
