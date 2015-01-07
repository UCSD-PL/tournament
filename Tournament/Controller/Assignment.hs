{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Assignment(assignmentRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Tournament.Database
import Tournament.Controller.Util
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status
import Web.Scotty

assignmentRoutes = do
    get "/courses/:id/assignments" $ do
       id <- param "id"
       assignments <- liftIO $ getAssignments id
       json assignments

    get "/courses/:id/assignments/:aid" $ do
       id <- param "id"
       aid <- param "aid"
       assignment <- liftIO $ getAssignment id aid
       maybe (status status404) json assignment

    post "/courses/:id/assignments" $ jsonParse "Invalid JSON" $ \(Assignment i _ n p) -> do
       id <- param "id"
       assignment <- liftIO $ insertAssignment (Assignment i id n p)
       json assignment

    delete "/courses/:id/assignments/:aid" $ do
       id <- param "id"
       aid <- param "aid"
       liftIO $ deleteAssignment id aid
       status status200
