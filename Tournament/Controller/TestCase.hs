{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.TestCase(testCaseRoutes) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Monoid (mconcat)
import Network.HTTP.Types.Status
import Tournament.Controller.Util
import Tournament.Database.TestCase
import Web.Scotty
import qualified Tournament.Database.Function as F

testCaseRoutes = do
    get "/courses/:id/assignments/:aid/functions/:fid/testCases" $ auth $ \user -> do
      fid <- param "fid"
      checkFunctionAuth user fid . const $ do
        testCases <- liftIO $ getTestCases fid
        json testCases

    get "/courses/:id/assignments/:aid/functions/:fid/testCases/:tcid" $ auth $ \user -> do
      tcid <- param "tcid"
      checkTestCaseAuth user tcid json

    post "/courses/:id/assignments/:aid/functions/:fid/testCases" $ jsonParse "Invalid JSON" $ \t ->
      auth $ \user -> do
        fid <- param "fid"
        checkFunctionAuth user fid . const $ do
          testCase <- liftIO . insertTestCase $ t { functionId = fid }
          json testCase

    delete "/courses/:id/assignments/:aid/functions/:fid/testCases/:tcid" $ auth $ \user -> do
      tcid <- param "tcid"
      checkTestCaseAuth user tcid . const $ do
        liftIO $ deleteTestCase tcid
        status status200
