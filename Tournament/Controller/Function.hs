{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Function(functionRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Tournament.Database.Function
import qualified Tournament.Database.Assignment as A
import Tournament.Controller.Util
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status
import Web.Scotty

functionRoutes = do
    get "/courses/:id/assignments/:aid/functions" $ auth $ \user -> do
      cid        <- param "id"
      aid        <- param "aid"
      assignment <- liftIO $ A.getAssignment cid aid
      okay       <- maybe (return False) (liftIO . checkAssignmentAuth user) assignment
      checkAuth okay $ do
        functions <- liftIO $ getFunctions aid
        json functions

    get "/courses/:id/assignments/:aid/functions/:fid" $ auth $ \user -> do
       fid      <- param "fid"
       function <- liftIO $ getFunction fid
       okay     <- maybe (return False) (liftIO . checkFunctionAuth user) function
       checkAuth okay $ maybe (status status404) json function

    post "/courses/:id/assignments/:aid/functions" $ jsonParse "Invalid JSON" $ \f ->
      auth $ \user -> do
        cid        <- param "id"
        aid        <- param "aid"
        assignment <- liftIO $ A.getAssignment cid aid
        okay       <- maybe (return False) (liftIO . checkAssignmentAuth user) assignment
        checkAuth okay $ do
          function <- liftIO . insertFunction $ f { assignmentId = aid }
          json function

    delete "/courses/:id/assignments/:aid/functions/:fid" $ auth $ \user -> do
      fid      <- param "fid"
      function <- liftIO $ getFunction fid
      okay     <- maybe (return False) (liftIO . checkFunctionAuth user) function
      checkAuth okay $ do
        liftIO $ deleteFunction fid
        status status200
