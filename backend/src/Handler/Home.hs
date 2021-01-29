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
  "Devnull org is a software development company specialized in \
   \ building teams capable of using various tech stacks and solving crucial problems for your business. \

   \ We prefer to work with Haskell programming language because it provides us with a way of building \
   \ composable code quickly and with high degree of certainty of correctness.  \

   \ If this is not possible because of various reasons our experts are capable of writing production level \
   \ code in any of the industry standard programming languages like C++, Java, Swift or even PHP and Javascript.\
   \ "

additionalText :: Text
additionalText =
  "Additionally we work on tools that should solve some of the pain points various companies experience in their daily operations. \
   \ Products \
   \ We continuously work on set of tools that could be of interest to either working developers or companies working mainly with web tech. You can learn more on the products page."
