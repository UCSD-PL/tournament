{-# LANGUAGE OverloadedStrings          #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Monoid (mconcat)
import Network.HTTP.Types.Status
import Tournament.Controller.Assignment
import Tournament.Controller.Course
import Tournament.Controller.Function
import Tournament.Controller.User
import Tournament.Controller.Util
import Web.Scotty

main = scotty 3000 $ do
    assignmentRoutes
    courseRoutes
    functionRoutes
    userRoutes
