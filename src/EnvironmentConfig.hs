module EnvironmentConfig (getApiConfiguration, ApiConfiguration(..)) where 

import Configuration.Dotenv ( loadFile, defaultConfig )
import Control.Exception
import qualified Control.Exception as Ex
import Data.List
import Control.Monad.Trans.Except
import qualified Control.Applicative as IO

type EnvironmentValuePair = [(String, String)]
getEnvironmentConfig :: IO (Either String EnvironmentValuePair)
getEnvironmentConfig = do
    catch (Right <$> loadFile defaultConfig) handleError
    where   handleError :: Ex.IOException -> IO (Either String EnvironmentValuePair)
            handleError = \_ -> pure $ Left "Unable to load environment configuration. Are you sure a dotenv file is present?"

data ApiConfiguration = 
    ApiConfiguration { hostName :: String
                     , username :: String
                     , password :: String
                     } deriving (Show)

matchFirst :: String -> (String, String) -> Bool 
matchFirst x (y, _) = x == y

toApiConfig :: Maybe String -> Maybe String -> Maybe String -> Either String ApiConfiguration
toApiConfig (Just host) (Just username) (Just password) = Right $ ApiConfiguration host username password
toApiConfig _ _ _                                       = Left "Missing environment configuration"

getApiConfiguration :: IO (Either String ApiConfiguration)
getApiConfiguration = runExceptT $ do
    config <- ExceptT getEnvironmentConfig
    let hostName = snd <$> getHostName config
    let username = snd <$> getUserName config
    let password = snd <$> getPassword config
    ExceptT $ IO.pure $ toApiConfig hostName username password 

    where getHostName           = find (matchFirst "HOST_NAME")
          getUserName           = find (matchFirst "USER_NAME")
          getPassword           = find (matchFirst "PASSWORD")
          


