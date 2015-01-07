{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Util where

import Data.Aeson (FromJSON)
import Data.Text.Lazy
import Network.HTTP.Types.Status
import Web.Scotty

jsonParse :: FromJSON a => Text -> (a -> ActionM ()) -> ActionM ()
jsonParse e b = go `rescue` handle
  where go = do d <- jsonData
                b d
        handle jsonError
          | "no parse" `isInfixOf` jsonError = status (mkStatus 422 "Unprocessable Entity") >> text e
          | otherwise = raise jsonError
