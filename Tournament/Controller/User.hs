{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.User(userRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Tournament.Database.User
import Tournament.Controller.Util
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status
import Web.Scotty

userRoutes =
    post "/users" $ jsonParse "Invalid JSON" $ \user -> do
        user <- liftIO $ createUser user
        json user
