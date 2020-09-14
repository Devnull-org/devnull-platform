{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Frontend where

import           Common.Route
import           Control.Lens                ((%~), (.~))
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class      (MonadIO)
import qualified Data.Aeson                  as A
import           Data.Aeson.TH
import           Data.Bool                   (bool)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle (MonadJSM, eval, liftJSM)
-- import           Nav
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend      (RouteToUrl, SetRoute, routeLink,
                                              subRoute_)
import           Reflex.Dom.Core
import           Reflex.Dom.Xhr              (postJson)

data ReceiveJson =
  ReceiveJson
    { receiveJsonUserId    :: Int
    , receiveJsonId        :: Int
    , receiveJsonTitle     :: Text
    , receiveJsonCompleted :: Bool
   } deriving Show

$(deriveJSON defaultOptions ''ReceiveJson )


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = subRoute_ $ \case
      FrontendRoute_Main -> body
      FrontendRoute_Contact -> contactBody
  }

contactBody
  :: ( MonadFix m
     , MonadHold t m
     , PostBuild t m
     , Prerender js t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute)  m
     )
  => m ()
contactBody =
  divClass "container" $ do
    header
    prerender_ blank contactForm
    return ()

body
  :: ( MonadFix m
     , MonadHold t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute)  m
     , DomBuilder t m)
  => m ()
body =
  divClass "container" $ do
    header
    divClass "col-md-12" $ do
      el "h1" (text "Haskell consultancy")
      mainContent
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: Text)
      -- prerender_ blank ajax
      return ()

menu
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     , Prerender js t m
     )
  => m ()
menu = do
  divClass "container" $
    divClass "row" $ do
      prerender_ blank $ do
        elAttr "button" ("class" =: "btn btn-default") $ routeLink (FrontendRoute_Main :/ ()) $ text "Home"
        elAttr "button" ("class" =: "btn btn-default") $ routeLink (FrontendRoute_Contact :/ ()) $ text "Contact"

header
  :: ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute)  m
     , Prerender js t m
     )
  => m ()
header = do
  elAttr "img" ("src" =: static @"devnull_logo.png") blank
  menu

contactForm
  :: ( MonadHold t m
     , MonadFix m
     , DomBuilder t m
     , PostBuild t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => m ()
contactForm = do
  rec
    divClass "form-group" $
      elClass "fieldset" "form-group" $ do
        inputChange <- mkInput "form-control" "Email"
        let inputValue = _inputElement_value inputChange
        evSubmit <- buttonDynClass (constDyn "btn btn-primary pull-right") (constDyn False) (text "Send")
        let receiveJson =
              ReceiveJson
                { receiveJsonUserId    = 1
                , receiveJsonId        = 100
                , receiveJsonTitle     = "" -- inputValue
                , receiveJsonCompleted = False
                }
        post <- curlPost "http://localhost/contact" receiveJson
        el "br" blank
        dynText =<< holdDyn "" post
  return ()

curlPost
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , PerformEvent t m
     , A.ToJSON a
     , TriggerEvent t m
     )
  => Text
  -> a
  -> m (Event t Text)
curlPost url json = do
  let req = postJson url json
  pb <- getPostBuild
  asyncReq <- performRequestAsync (tag (constant req) pb)
  pure $ fmap (fromMaybe "Unknown error" . _xhrResponse_responseText) asyncReq

ajax
  :: ( HasJSContext (Performable m)
     , PostBuild t m
     , MonadIO m
     , DomBuilder t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     ) => m ()
ajax = do
  el "div" $ do
    -- you can either create a fake event and then trigger it or use a button event for example and then
    -- fire ajax call on button click
    -- (fake_event,trigger) <- newTriggerEvent
    -- resp :: Event t (Maybe ReceiveJson) <- getAndDecode (const ("https://jsonplaceholder.typicode.com/todos/1") <$> fake_event)
    -- liftIO $ trigger ()
    buttonEvent <- buttonDynClass (constDyn "btn btn-primary pull-right") (constDyn False) (text "Send ajax")
    resp :: Event t (Maybe ReceiveJson) <- getAndDecode ((const "https://jsonplaceholder.typicode.com/todos/1") <$> buttonEvent)
    responseDyn <- holdDyn Nothing (Just <$> resp)
    display responseDyn

-- | Function for genericly doing most json post request
genericJsonPost :: (MonadWidget t m, A.ToJSON c, A.FromJSON b) => Event t a -> (a -> Text) -> (a -> c) -> m (Event t (Maybe b))
genericJsonPost trigger toUrl toData =
  fmap decodeXhrResponse <$> performRequestAsync req
  where
    req = toReq <$> trigger
    toReq a = postJson (toUrl a) (toData a)

mkInput :: DomBuilder t m => Text -> Text -> m (InputElement EventResult (DomBuilderSpace m) t)
mkInput className placeHolder =
    inputElement $ def
      & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
        [ ("class", className)
        , ("placeholder", placeHolder)
        ])

buttonClass
  :: forall t m a
  . (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t Bool
  -> m a
  -> m (Event t ())
buttonClass cls = buttonDynClass (constDyn cls)

buttonDynClass
  :: forall t m a
  . (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Bool
  -> m a
  -> m (Event t ())
buttonDynClass clsDyn disabledDyn m = do
  let attrsDyn = (<>) <$> (("class" =:) <$> clsDyn) <*> (bool (Map.empty) ("disabled" =: "") <$> disabledDyn)
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_modifyAttributes .~ modAttrs
  (e, _) <- element "button" cfg m
  pure $ domEvent Click e

inputDynClass
  :: forall t m a
  . (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Bool
  -> m a
  -> m (Event t ())
inputDynClass clsDyn disabledDyn m = do
  let attrsDyn = (<>) <$> (("class" =:) <$> clsDyn) <*> (bool (Map.empty) ("disabled" =: "") <$> disabledDyn)
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_modifyAttributes .~ modAttrs
  (e, _) <- element "input" cfg m
  pure $ domEvent Change e

mainContent :: DomBuilder t m => m ()
mainContent = do
  el "ul" $ do
    el "li" (text "writing production level code")
    el "li" (text "code optimization")
    el "li" (text "debugging and fixing problems with execution speed and/or large memory consumption")
  elClass "p" "col-md-12" (text bodyText)
  where
    bodyText :: Text
    bodyText =
      "Additionally we work on tools that should solve some of the pain points various companies experience in their daily operations. \
      \ Products \
      \ We continuously work on set of tools that could be of interest to either working developers or companies working mainly with web tech. You can learn more on the products page."
