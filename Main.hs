{-# LANGUAGE OverloadedStrings          #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Monoid (mconcat)
import Tournament.Controller.Assignment
import Tournament.Controller.Course
import Tournament.Controller.Util
import Network.HTTP.Types.Status
import Web.Scotty

main = scotty 3000 $ do
    courseRoutes
    assignmentRoutes
