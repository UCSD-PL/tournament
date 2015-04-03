{-# LANGUAGE OverloadedStrings          #-}

module Tournament.Controller.Util where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.ByteString.Base64 (decodeLenient)
import Data.Text.Lazy (Text, isInfixOf)
import Database.HDBC
import Database.HDBC.PostgreSQL
import Network.HTTP.Types.Status
import Network.Wai (Request, requestHeaders)
import Tournament.Database.Util
import Web.Scotty hiding (notFound)
import qualified Data.ByteString.Char8 as BS
import qualified Tournament.Database.Assignment as A
import qualified Tournament.Database.Course as C
import qualified Tournament.Database.Function as F
import qualified Tournament.Database.User as U
import qualified Tournament.Database.TestCase as T

jsonParse :: FromJSON a => Text -> (a -> ActionM ()) -> ActionM ()
jsonParse e b = go `rescue` handle
  where go = do d <- jsonData
                b d
        handle jsonError
          | "no parse" `isInfixOf` jsonError = status (mkStatus 422 "Unprocessable Entity") >> text e
          | otherwise = raise jsonError

unBase64 :: BS.ByteString -> Maybe (String, String)
unBase64 x = case BS.split ' ' x of
  (_ : thing : _) -> case (fmap BS.unpack . BS.split ':' . decodeLenient) thing of
                       [l, p] -> Just (l, p)
                       _      -> Nothing
  _               -> Nothing
  

auth :: (U.User -> ActionM ()) -> ActionM ()
auth action = do
  r <- request
  let h = requestHeaders r
  case [v | (k, v) <- h, k == "Authorization" ] of
   [value] ->
     case unBase64 value of
      Just (l, p) -> do
         user <- liftIO $ check (l, p)
         case user of
          Nothing -> unauthorized
          Just u  -> action u
      _ -> unauthorized
   _       -> unauthorized

  where
    check (login, pwd) = U.loginUser login pwd

checkAssignmentAuth :: U.User -> Int -> Int -> (A.Assignment -> ActionM ()) -> ActionM ()
checkAssignmentAuth u cid aid action = do
  assignemnt <- liftIO $ A.getAssignment cid aid
  case assignemnt of
   Nothing -> notFound
   Just a -> do
     res <- liftIO . withDatabase $ \conn -> quickQuery' conn query [toSql (U.id u), toSql (A.id a)]
     if length res == 1
       then action a
       else unauthorized
  where
    query = "SELECT 1 FROM courses c, assignments a WHERE a.courseId = c.id AND c.userId = ? AND a.id = ?"

checkFunctionAuth :: U.User -> Int -> (F.Function -> ActionM ()) -> ActionM ()
checkFunctionAuth u fid a = do
  function <- liftIO $ F.getFunction fid
  case function of
   Nothing -> notFound
   Just f -> do
     res <- liftIO . withDatabase $ \conn -> quickQuery' conn query [toSql (U.id u), toSql (F.id f)]
     if length res == 1
       then a f
       else unauthorized
  where
    query = "SELECT 1 FROM courses c, assignments a, functions f WHERE a.courseId = c.id AND f.assignmentId = a.id AND c.userId = ? AND f.id = ?"

checkTestCaseAuth :: U.User -> Int -> (T.TestCase -> ActionM ()) -> ActionM ()
checkTestCaseAuth u tcid a = do
  testCase <- liftIO $ T.getTestCase tcid
  case testCase of
   Nothing -> notFound
   Just tc -> do
     res <- liftIO . withDatabase $ \conn -> quickQuery' conn query [toSql (U.id u), toSql (T.id tc)]
     if length res == 1
       then a tc
       else unauthorized
  where
    query = "SELECT 1 FROM courses c, assignments a, functions f, testCases t WHERE a.courseId = c.id AND f.assignmentId = a.id AND t.functionId = f.id AND c.userId = ? AND t.id = ?"

checkAuth :: Bool -> ActionM () -> ActionM ()
checkAuth True a = a
checkAuth False _ = unauthorized

unauthorized = status (mkStatus 401 "Unauthorized") >> text "You are not authorized"
notFound = status (mkStatus 404 "Not Found") >> text "Ain't there yo"
