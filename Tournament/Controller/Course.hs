{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Course(courseRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Monoid (mconcat)
import Network.HTTP.Types.Status
import Tournament.Controller.Util
import Tournament.Database.Course
import Web.Scotty
import qualified Tournament.Database.User as U

courseRoutes = do
    get "/courses" $ auth $ \user -> do
        courses <- liftIO $ getCourses (U.id user)
        json courses

    get "/courses/:id" $ auth $ \user -> do
        cid <- param "id"
        course <- liftIO $ getCourse cid
        maybe (status status404) json course

    delete "/courses/:id" $ auth $ \user -> do
        id <- param "id"
        course <- liftIO $ getCourse id
        maybe (status status404) (performDelete user) course

    post "/courses" $ jsonParse "Invalid JSON" $ \course ->
      auth $ \user -> do
        course <- liftIO $ insertCourse (course { userId = (U.id user)})
        json course

performDelete user course
  | (U.id user) == (userId course) = do
      liftIO $ deleteCourse (Tournament.Database.Course.id course)
      status status200
  | otherwise                      = unauthorized
