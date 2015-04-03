{-# LANGUAGE OverloadedStrings #-}

module Tournament.Database.Course(Course(..), getCourses, getCourse, deleteCourse, insertCourse) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Tournament.Database.Util

data Course = Course { id :: Int
                     , userId :: Int
                     , code :: String
                     , name :: String
                     , term :: String
                     }

instance FromJSON Course where
 parseJSON (Object v) =
    Course <$> v .:? "id" .!= 0
           <*> v .: "userId"
           <*> v .: "department"
           <*> v .: "course"
           <*> v .: "term"
 parseJSON _ = mzero

instance ToJSON Course where
 toJSON (Course id userId department course term) =
    object [ "id" .= id
           , "userId" .= userId
           , "department" .= department
           , "course"     .= course
           , "term"       .= term
           ]

toCourse :: [SqlValue] -> Course
toCourse (id : user : department : course : prof : _) = Course (fromSql id) (fromSql user) (fromSql department) (fromSql course) (fromSql prof)

getCourses :: Int -> IO [Course]
getCourses u = withDatabase $ \conn -> do
                courses <- quickQuery' conn "SELECT * FROM courses WHERE userId = ?" [toSql u]
                return $ map toCourse courses

getCourse :: Int -> IO (Maybe Course)
getCourse a = withDatabase $ \conn -> do
                 course <- quickQuery' conn "SELECT * FROM courses WHERE id=?" [toSql a]
                 return $ fmap toCourse . listToMaybe $ course

deleteCourse :: Int -> IO Integer
deleteCourse a = withDatabase $ \conn -> run conn "DELETE FROM courses WHERE id=?" [toSql a]

insertCourse :: Course -> IO Course
insertCourse (Course _ u a b c) = withDatabase $ \conn -> do
                                 res <- quickQuery' conn "INSERT INTO courses(userId, department, course, term) VALUES (?, ?, ?) RETURNING id" [toSql u, toSql a, toSql b, toSql c]
                                 let ((id : _) : _) = res
                                 return $ Course (fromSql id) u a b c
