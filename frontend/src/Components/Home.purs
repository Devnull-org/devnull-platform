module Components.Home
  ( homeComponent
  ) where

import Data.Maybe (fromMaybe)
import Data.String.NonEmpty (unsafeFromString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Pathy.Name (Name(..))
import Pathy.Path (rootDir, (</>), dir', file')
import Pathy.Printer (printPath, posixPrinter)
import Pathy.Sandboxed (sandbox)
import Prelude

type State =
    { loading :: Boolean
    , content :: String
    }

data Action
       = DownloadingContent
       | ReceivedContent String

data Message = ReceivedData String

homeComponent :: forall q i o m. H.Component HH.HTML q i o m
homeComponent =
  H.mkComponent
    { initialState: initialState
    , render: ui
    , eval: H.mkEval H.defaultEval
    }

initialState :: forall i. i -> State
initialState _ =
  { loading : false
  , content : ""
  }

ui :: forall a b. State -> HH.HTML a b
ui _state = do
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
              [ HH.text "Devnull org is a software consultancy company specialized in \
                       \ working with Haskell programming language. \
                       \ We work with Haskell because it provides us with a way of building \
                       \ composable code quickly and with high degree of certainty of correctness. \
                       \ "
              ]
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

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction action =
  case action of
    DownloadingContent ->
      H.modify_ \st -> st { loading = true, content = ""}
    ReceivedContent receivedContent ->
      H.modify_ \st ->
        st { loading = false
           , content = receivedContent
           }
