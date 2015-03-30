{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Util where

import Data.Aeson (FromJSON)
import Data.Text.Lazy (Text, isInfixOf)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base64 (decodeLenient)
import qualified Tournament.Database.User as U
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Tournament.Database.Course as C
import Tournament.Database.Util
import qualified Tournament.Database.Assignment as A
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request, requestHeaders)
import Network.HTTP.Types.Status
import Web.Scotty

jsonParse :: FromJSON a => Text -> (a -> ActionM ()) -> ActionM ()
jsonParse e b = go `rescue` handle
  where go = do d <- jsonData
                b d
        handle jsonError
          | "no parse" `isInfixOf` jsonError = status (mkStatus 422 "Unprocessable Entity") >> text e
          | otherwise = raise jsonError

auth :: (U.User -> ActionM ()) -> ActionM ()
auth action = do
  r <- request
  let h = requestHeaders r
  case [v | (k, v) <- h, k == "Authorization" ] of
   [value] -> do
     okay <- liftIO $ check (decode value)
     case okay of
      Nothing -> unauthorized
      Just u  -> action u
   _       -> unauthorized

  where
    decode = fmap BS.unpack . BS.split ':' . decodeLenient . head . drop 1 . BS.split ' '
    check [login, pwd] = U.loginUser login pwd
    check _            = return Nothing

checkAssignmentAuth :: U.User -> A.Assignment -> IO Bool
checkAssignmentAuth u a = withDatabase $ \conn -> do
  res <- quickQuery' conn query [toSql (U.id u), toSql (A.id a)]
  return $ length res == 1
  where
    query = "SELECT 1 FROM courses c, assignments a WHERE a.courseId = c.id AND c.userId = ? AND a.id = ?"

checkAuth :: Bool -> ActionM () -> ActionM ()
checkAuth True a = a
checkAuth False _ = unauthorized

unauthorized = status (mkStatus 401 "Unauthorized") >> text "You are not authorized"
