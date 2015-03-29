{-# LANGUAGE OverloadedStrings #-}

module Tournament.Database.TestCase(
  TestCase(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Tournament.Database.Util

data TestCase = TestCase { testCaseId :: Integer
                         , functionId :: Integer
                         , userId :: Integer
                         , args :: String
                         , attempts :: Integer
                         , failures :: Integer
                         }

instance FromJSON TestCase where
  parseJSON (Object v) =
    TestCase <$> v .:? "id" .!= 0
             <*> v .: "functionId"
             <*> v .: "userId"
             <*> v .: "args"
             <*> v .: "attempts"
             <*> v .: "failures"
  parseJSON _ = mzero

instance ToJSON TestCase where
  toJSON f =
    object [ "id"         .= testCaseId f
           , "functionId" .= functionId f
           , "userId"     .= userId f
           , "args"       .= args f
           , "attempts"   .= attempts f
           , "failures"   .= failures f
           ]

toTestCase                                                             :: [SqlValue] -> Maybe TestCase
toTestCase (id : functionId : userId : args : attempts : failures : _) = Just $ TestCase (fromSql id) (fromSql functionId) (fromSql userId) (fromSql args) (fromSql attempts) (fromSql failures)
toTestCase _                                                           = Nothing

getTestCase            :: Int -> IO (Maybe TestCase)
getTestCase testCaseId = withDatabase $ \conn -> do
  res <- quickQuery' conn "SELECT * FROM testCases WHERE id=?" [toSql testCaseId]
  return $ toFunction =<< listToMaybe res

insertTestCase :: TestCase -> IO Function
insertTestCase (TestCase _ a b c _ _) = withDatabase $ \conn -> do
                                 res <- quickQuery' conn "INSERT INTO testCases(functionId, userId, args) VALUES (?, ?, ?) RETURNING id" [toSql a, toSql b, toSql c]
                                 let id = fromSql . head . head $ res -- fuck...
                                 return $ Function id a b c

deleteTestCase :: Int -> IO Integer
deleteTestCase functionId = withDatabase $ \conn -> run conn "DELETE FROM testCases WHERE id=?" [toSql functionId]
