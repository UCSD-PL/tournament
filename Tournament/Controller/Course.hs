{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Course(courseRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Tournament.Database.Course
import Tournament.Controller.Util
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status
import Web.Scotty

courseRoutes = do
    get "/courses" $ do
        courses <- liftIO getCourses
        json courses

    get "/courses/:id" $ do
        id <- param "id"
        course <- liftIO $ getCourse id
        maybe (status status404) json course

    delete "/courses/:id" $ do
        id <- param "id"
        liftIO $ deleteCourse id
        status status200

    post "/courses" $ jsonParse "Invalid JSON" $ \course -> do
        course <- liftIO $ insertCourse course
        json course
