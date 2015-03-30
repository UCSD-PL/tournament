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
      checkAssignmentAuth user cid aid . const $ do
        functions <- liftIO $ getFunctions aid
        json functions

    get "/courses/:id/assignments/:aid/functions/:fid" $ auth $ \user -> do
       fid      <- param "fid"
       checkFunctionAuth user fid json

    post "/courses/:id/assignments/:aid/functions" $ jsonParse "Invalid JSON" $ \f ->
      auth $ \user -> do
        cid        <- param "id"
        aid        <- param "aid"
        checkAssignmentAuth user cid aid . const $ do
          function <- liftIO . insertFunction $ f { assignmentId = aid }
          json function

    delete "/courses/:id/assignments/:aid/functions/:fid" $ auth $ \user -> do
      fid      <- param "fid"
      checkFunctionAuth user fid . const $ do
        liftIO $ deleteFunction fid
        status status200
