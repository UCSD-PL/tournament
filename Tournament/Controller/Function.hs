{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Function(functionRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Tournament.Database.Function
import Tournament.Controller.Util
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status
import Web.Scotty

functionRoutes = do
    get "/courses/:id/assignments/:aid/functions" $ do
       aid <- param "aid"
       functions <- liftIO $ getFunctions aid
       json functions

    get "/courses/:id/assignments/:aid/functions/:fid" $ do
       fid <- param "fid"
       function <- liftIO $ getFunction fid
       maybe (status status404) json function

    post "/courses/:id/assignments/:aid/functions" $ jsonParse "Invalid JSON" $ \f -> do
       aid <- param "aid"
       function <- liftIO . insertFunction $ f { assignmentId = aid }
       json function

    delete "/courses/:id/assignments/:aid/functions/:fid" $ do
       fid <- param "fid"
       liftIO $ deleteFunction fid
       status status200
