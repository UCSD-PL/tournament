{-# LANGUAGE OverloadedStrings #-}

module Tournament.Database.Assignment(
  Assignment(..),
  getAssignments,
  getAssignment,
  insertAssignment,
  deleteAssignment
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Tournament.Database.Util

data Assignment = Assignment Int Int String Int

instance FromJSON Assignment where
  parseJSON (Object v) =
    Assignment <$> v .:? "id" .!= 0
               <*> v .:? "courseId" .!= 0
               <*> v .: "name"
               <*> v .: "maxPoints"
  parseJSON _ = mzero

instance ToJSON Assignment where
  toJSON (Assignment id courseId name maxPoints) =
    object [ "id"         .= id
           , "courseId"   .= courseId
           , "name"       .= name
           , "maxPoints"  .= maxPoints
           ]

toAssignment :: [SqlValue] -> Assignment
toAssignment (id : courseId : name : maxPoints : _) = Assignment (fromSql id) (fromSql courseId) (fromSql name) (fromSql maxPoints)

getAssignments :: Int -> IO [Assignment]
getAssignments courseId = withDatabase $ \conn -> do
                            res <- quickQuery' conn "SELECT * FROM assignments WHERE courseId=?" [toSql courseId]
                            return $ map toAssignment res

getAssignment :: Int -> Int -> IO (Maybe Assignment)
getAssignment courseId aid = withDatabase $ \conn -> do
                                res <- quickQuery' conn "SELECT * FROM assignments WHERE courseId=? AND id=?" [toSql courseId, toSql aid]
                                return $ fmap toAssignment . listToMaybe $ res

insertAssignment :: Assignment -> IO Assignment
insertAssignment (Assignment _ a b c) = withDatabase $ \conn -> do
                                 res <- quickQuery' conn "INSERT INTO assignments(courseId, name, maxPoints) VALUES (?, ?, ?) RETURNING id" [toSql a, toSql b, toSql c]
                                 let id = fromSql . head . head $ res
                                 return $ Assignment id a b c

deleteAssignment :: Int -> Int -> IO Integer
deleteAssignment courseId aid = withDatabase $ \conn -> run conn "DELETE FROM assignments WHERE courseId=? AND id=?" [toSql courseId, toSql aid]
