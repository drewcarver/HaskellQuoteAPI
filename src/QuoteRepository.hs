{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module QuoteRepository (getQuotes) where
import Database.MongoDB    (PortID(PortNumber), AccessMode (UnconfirmedWrites), Action, Document, Value, Pipe, Database,
                            access,
                            close, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            openReplicaSetSRV', primary, select, sort, (=:),
                            auth)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except

connectToAtlas :: IO Pipe
connectToAtlas = openReplicaSetSRV' "<connectionstring>" >>= primary

authenticateAccess :: Pipe -> IO Bool
authenticateAccess pipe = access pipe master "admin" $ auth "<username>" "<password>"

runQuery :: Pipe -> Action IO m -> IO m
runQuery pipe query = access pipe UnconfirmedWrites "home-automation-dashboard" query

connect :: Action IO [m] -> IO [m]
connect query = do
  pipe <- connectToAtlas
  isAuthSuccessful <- authenticateAccess pipe
  result <- if isAuthSuccessful 
      then runQuery pipe query
      else pure [] 
  close pipe
  pure result

findQuotes :: Action IO [Document]
findQuotes = rest =<< find (select [] "quotes")

getQuotes :: IO [Document]
getQuotes = connect findQuotes
