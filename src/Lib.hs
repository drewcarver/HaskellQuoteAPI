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
import Data.AesonBson
import Network.Wai
import Network.Wai.Handler.Warp
import QuoteRepository
import Data.Bson.Generic 
import Data.Maybe (catMaybes)
import Database.MongoDB (Document)
import GHC.Generics (Generic)
import Web.Scotty (ActionM, scotty)
import Web.Scotty.Trans 
import Control.Monad.IO.Class (MonadIO(liftIO))

data Quote = Quote
  { author  :: String
  , exerpt  :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Quote)
instance ToBSON Quote
instance FromBSON Quote

quotesList :: [Quote] -> ActionM ()
quotesList quotes = json quotes

transformQuotes :: [Document] -> [Quote]
transformQuotes docs = catMaybes (map fromBSON docs) 

getAndTransformFromRepository :: IO [Quote]
getAndTransformFromRepository = do
  quotes <- getQuotes
  pure (transformQuotes quotes)

startApp :: IO ()
startApp = scotty 3000 $
  get "/quotes" $ do 
    quotes <- liftIO $ getAndTransformFromRepository
    quotesList quotes
    
  