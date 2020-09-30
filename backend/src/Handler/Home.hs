{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Home
  ( HomeResponse (..)
  , homeHandler
  ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Text     (Text)
import           Prelude
import           Types         (AppM)

data HomeResponse =
  HomeResponse
    { homeResponseTitle    :: Text
    , homeResponseMainText :: Text
    , homeResponseAdditionalText :: Text
    } deriving (Show)

$(deriveJSON defaultOptions ''HomeResponse)

homeHandler :: AppM HomeResponse
homeHandler = do
  return
    HomeResponse
      { homeResponseTitle    = "Devnull org"
      , homeResponseMainText = mainText
      , homeResponseAdditionalText = additionalText
      }

mainText :: Text
mainText =
  "Devnull org is a software consultancy company specialized in \
   \ working with Haskell programming language. \
   \ We work with Haskell because it provides us with a way of building \
   \ composable code quickly and with high degree of certainty of correctness. \
   \ "

additionalText :: Text
additionalText =
  "Additionally we work on tools that should solve some of the pain points various companies experience in their daily operations. \
   \ Products \
   \ We continuously work on set of tools that could be of interest to either working developers or companies working mainly with web tech. You can learn more on the products page."
