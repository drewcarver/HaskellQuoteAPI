{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( startApp
    , Quote
    ) where

import Data.Aeson.TH
import QuoteRepository
import Data.Bson.Generic 
import Data.Maybe (mapMaybe)
import Database.MongoDB (Document)
import GHC.Generics (Generic)
import Web.Scotty (ActionM, scotty)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Web.Scotty.Trans 
import Control.Monad.IO.Class (MonadIO(liftIO))
import EnvironmentConfig
import Control.Monad.Trans.Except

data Quote = Quote
  { author  :: String
  , exerpt  :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Quote)
instance ToBSON Quote
instance FromBSON Quote

quotesList :: [Quote] -> ActionM ()
quotesList = json 

transformQuotes :: [Document] -> [Quote]
transformQuotes = mapMaybe fromBSON 

getAndTransformFromRepository :: ApiConfiguration -> IO (Either String [Quote])
getAndTransformFromRepository apiConfig = do
    quotes <- getQuotes hostName' username' password'
    let result = case quotes of 
          Right q -> Right $ transformQuotes q
          Left  e -> Left e
    return result
    where [hostName', username', password'] = [hostName, username, password] <*> pure apiConfig

listQuotes :: IO (Either String [Quote])
listQuotes = runExceptT $ do
    appConfig <- ExceptT getApiConfiguration 
    ExceptT $ getAndTransformFromRepository appConfig

startApp :: IO ()
startApp = scotty 3000 $
  get "/quotes" $ do 
    quotes <- liftIO listQuotes
    case quotes of
      Right q -> quotesList q
      Left  e -> text $ L.fromStrict $ T.pack e 
  