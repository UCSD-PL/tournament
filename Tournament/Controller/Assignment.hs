{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Assignment(assignmentRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Tournament.Database.Assignment
import qualified Tournament.Database.Assignment as A
import qualified Tournament.Database.Course as C
import qualified Tournament.Database.User as U
import Tournament.Controller.Util
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status
import Web.Scotty

assignmentRoutes = do
    get "/courses/:id/assignments" $ auth $ \_ -> do
       id <- param "id"
       assignments <- liftIO $ A.getAssignments id
       json assignments

    get "/courses/:id/assignments/:aid" $ auth $ \_ -> do
       id <- param "id"
       aid <- param "aid"
       assignment <- liftIO $ A.getAssignment id aid
       maybe (status status404) json assignment

    post "/courses/:id/assignments" $ jsonParse "Invalid JSON" $ \a ->
      auth $ \user -> do
        cid <- param "id"
        course <- liftIO $ C.getCourse cid
        checkAuth (maybe False ((== U.id user) . C.userId) course) $ do
          assignment <- liftIO . A.insertAssignment $ a { courseId = cid }
          json assignment

    delete "/courses/:id/assignments/:aid" $ auth $ \user -> do
      cid <- param "id"
      aid <- param "aid"
      assignment <- liftIO $ getAssignment cid aid
      okay <- maybe (return False) (liftIO . checkAssignmentAuth user) assignment
      checkAuth okay $ do
        liftIO $ A.deleteAssignment cid aid
        status status200
