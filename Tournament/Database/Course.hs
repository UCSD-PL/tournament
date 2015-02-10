{-# LANGUAGE OverloadedStrings #-}

module Tournament.Database.Course(Course(..), getCourses, getCourse, deleteCourse, insertCourse) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Tournament.Database.Util

data Course = Course { id :: Integer
                     , code :: String
                     , name :: String
                     , term :: String
                     }

instance FromJSON Course where
 parseJSON (Object v) =
    Course <$> v .:? "id" .!= 0
           <*> v .: "department"
           <*> v .: "course"
           <*> v .: "term"
 parseJSON _ = mzero

instance ToJSON Course where
 toJSON (Course id department course term) =
    object [ "id" .= id
           , "department" .= department
           , "course"     .= course
           , "term"       .= term
           ]

toCourse :: [SqlValue] -> Course
toCourse (id : department : course : prof : _) = Course (fromSql id) (fromSql department) (fromSql course) (fromSql prof)

getCourses :: IO [Course]
getCourses = withDatabase $ \conn -> do
                courses <- quickQuery' conn "SELECT * FROM courses" []
                return $ map toCourse courses

getCourse :: Int -> IO (Maybe Course)
getCourse a = withDatabase $ \conn -> do
                 course <- quickQuery' conn "SELECT * FROM courses WHERE id=?" [toSql a]
                 return $ fmap toCourse . listToMaybe $ course

deleteCourse :: Int -> IO Integer
deleteCourse a = withDatabase $ \conn -> run conn "DELETE FROM courses WHERE id=?" [toSql a]

insertCourse :: Course -> IO Course
insertCourse (Course _ a b c) = withDatabase $ \conn -> do
                                 res <- quickQuery' conn "INSERT INTO courses(department, course, term) VALUES (?, ?, ?) RETURNING id" [toSql a, toSql b, toSql c]
                                 let id = fromSql . head . head $ res
                                 return $ Course id a b c
