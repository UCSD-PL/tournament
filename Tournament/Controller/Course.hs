{-# LANGUAGE OverloadedStrings #-}

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
        let newId = secured (U.id user) user
        courses <- liftIO $ getCourses newId
        json courses

    get "/courses/:id" $ auth $ \user -> do
        cid <- param "id"
        let newId = secured cid user
        course <- liftIO $ getCourse newId
        maybe (status status404) json course

    delete "/courses/:id" $ auth $ \user -> do
        id <- param "id"
        let newId = secured id user
        course <- liftIO $ getCourse newId
        maybe (status status404) (performDelete user) course

    post "/courses" $ jsonParse "Invalid JSON" $ \course ->
      auth $ \user -> do
        let newCourse = secured (course { userId = U.id user }) user
        course <- liftIO $ insertCourse newCourse
        json course

performDelete user course
  | U.id user == userId course = do
      let deleteId = secured (Tournament.Database.Course.id course) user
      liftIO $ deleteCourse deleteId
      status status200
  | otherwise                      = unauthorized
