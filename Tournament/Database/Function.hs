{-# LANGUAGE OverloadedStrings #-}

module Tournament.Database.Function(
  getFunctions,
  getFunction,
  insertFunction,
  deleteFunction,
  Function(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Tournament.Database.Util

data Function = Function { id :: Int
                         , assignmentId :: Int
                         , description :: String
                         , instructions :: String
                         }

instance FromJSON Function where
  parseJSON (Object v) =
    Function <$> v .:? "id" .!= 0
             <*> v .:? "assignmentId" .!= 0
             <*> v .: "description"
             <*> v .: "instructions"
  parseJSON _ = mzero

instance ToJSON Function where
  toJSON f =
    object [ "id"           .= Tournament.Database.Function.id f
           , "assignmentId" .= assignmentId f
           , "description"  .= description f
           , "instructions" .= instructions f
           ]

toFunction                                                      :: [SqlValue] -> Maybe Function
toFunction (id : assignmentId : description : instructions : _) = Just $ Function (fromSql id) (fromSql assignmentId) (fromSql description) (fromSql instructions)
toFunction _                                                    = Nothing

getFunctions              :: Int -> IO [Function]
getFunctions assignmentId = withDatabase $ \conn -> do
  res <- quickQuery' conn "SELECT * FROM functions WHERE assignmentId=?" [toSql assignmentId]
  return $ mapMaybe toFunction res

getFunction            :: Int -> IO (Maybe Function)
getFunction functionId = withDatabase $ \conn -> do
  res <- quickQuery' conn "SELECT * FROM functions WHERE id=?" [toSql functionId]
  return $ toFunction =<< listToMaybe res

insertFunction :: Function -> IO Function
insertFunction (Function _ a b c) = withDatabase $ \conn -> do
                                 res <- quickQuery' conn "INSERT INTO functions(assignmentId, description, instructions) VALUES (?, ?, ?) RETURNING id" [toSql a, toSql b, toSql c]
                                 let id = fromSql . head . head $ res -- fuck...
                                 return $ Function id a b c

deleteFunction :: Int -> IO Integer
deleteFunction functionId = withDatabase $ \conn -> run conn "DELETE FROM functions WHERE id=?" [toSql functionId]
