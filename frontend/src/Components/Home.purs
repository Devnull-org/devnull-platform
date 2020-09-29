module Components.Home
  ( homeComponent
  ) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (fromMaybe, Maybe (..))
import Data.String.NonEmpty (unsafeFromString)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Pathy.Name (Name(..))
import Pathy.Path (rootDir, (</>), dir', file')
import Pathy.Printer (printPath, posixPrinter)
import Pathy.Sandboxed (sandbox)

type State =
    { loading :: Boolean
    , content :: String
    }

data Action
       = DownloadContent
       | ReceivedContent String

data Message = ReceivedData String

homeComponent :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
homeComponent =
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
  , content : "loading..."
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
              [HH.text "Devnull org"]
          , HH.p
              [ HP.class_ (H.ClassName "row bg-white p-4 border")]
              [ HH.text st.content]
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
              [ HH.text
                  "Additionally we work on tools that should solve some of the pain points various companies experience in their daily operations. \
                   \ Products \
                   \ We continuously work on set of tools that could be of interest to either working developers or companies working mainly with web tech. You can learn more on the products page."
              ]
          ]
    ]

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction action =
  case action of
    DownloadContent -> do
      response <- H.liftAff $ AX.get AXRF.string ("http://localhost:9009/home")
      H.modify_ (\st -> st { loading = false, content = fromMaybe "" (map _.body (hush response)) })
    ReceivedContent receivedContent ->
      H.modify_ \st ->
        st { loading = false
           , content = receivedContent
           }
