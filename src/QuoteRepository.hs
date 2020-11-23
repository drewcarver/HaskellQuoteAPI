{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module QuoteRepository (getQuotes) where
import Database.MongoDB    (AccessMode (UnconfirmedWrites), Action, Document, Pipe,
                            access, find, master, project, rest,
                            openReplicaSetSRV', primary, select, sort,
                            auth)
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Control.Exception
import qualified Control.Exception as Ex

type HostAddress = String
connectToAtlas :: HostAddress -> IO Pipe
connectToAtlas hostAddress = openReplicaSetSRV' hostAddress >>= primary

type Username = String
type Password = String
authenticateAccess :: Username -> Password -> Pipe -> IO Bool
authenticateAccess username password pipe = access pipe master "admin" $ auth packedUsername packedPassword 
  where packedUsername = T.pack username 
        packedPassword = T.pack password

runQuery :: Pipe -> Action IO m -> IO m
runQuery pipe = access pipe UnconfirmedWrites "home-automation-dashboard" 

type DatabaseError = String 

findQuotes :: Action IO [Document]
findQuotes = rest =<< find (select [] "quotes")

getMongoConnection :: HostAddress -> Username -> Password -> IO (Either String Pipe)
getMongoConnection host username pass = runExceptT $ do
  pipe <- ExceptT $ catch (Right <$> connectToAtlas host) (handler "Couldn't open pipe to mongo server.")
  isAuthSuccessful <- ExceptT $ Right <$> authenticateAccess username pass pipe
  ExceptT $ authHandler isAuthSuccessful pipe

  where handler :: String -> Ex.IOException -> IO (Either String Pipe)
        handler err = \_ -> pure (Left err)
        
        authHandler :: Bool -> Pipe -> IO (Either String Pipe)
        authHandler isAuthenticated pipe
          | isAuthenticated         = pure $ Right pipe 
          | not isAuthenticated     = pure $ Left "Unable to authenticate."

getQuotes :: HostAddress -> Username -> Password -> IO (Either DatabaseError [Document])
getQuotes host username pass = runExceptT $ do
  pipe <- ExceptT $ getMongoConnection host username pass 
  ExceptT $ Right <$> runQuery pipe findQuotes
