{-# LANGUAGE OverloadedStrings #-}

module Tournament.Database.User(
  User(..)
, createUser
, loginUser
) where

import Control.Applicative
import Control.Monad
import Crypto.Hash
import Data.Aeson
import Data.Byteable
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Tournament.Database.Util
import qualified Data.ByteString.Char8 as B

data User = User { id :: Int
                 , email :: String
                 , password :: String
                 }

instance FromJSON User where
 parseJSON (Object v) =
    User <$> v .:? "id" .!= 0
           <*> v .: "email"
           <*> v .: "password"
 parseJSON _ = mzero

instance ToJSON User where
 toJSON (User id email _) =
    object [ "id" .= id
           , "email" .= email
           ]

toUser :: [SqlValue] -> User
toUser (id : email : pass : _) = User (fromSql id) (fromSql email) (fromSql pass)

sha512d :: B.ByteString -> Digest SHA512
sha512d = hash

sha512 :: String -> String
sha512 = B.unpack . toBytes . sha512d . B.pack

createUser :: User -> IO User
createUser (User _ email pass) = withDatabase $ \conn -> do
  let encPass = sha512 pass
  res <- quickQuery' conn "INSERT INTO users(email, password) VALUES (?, ?) RETURNING id" [toSql email, toSql encPass]
  let id = fromSql . head . head $ res
  return $ User id email encPass

loginUser :: String -> String -> IO (Maybe User)
loginUser email pass = withDatabase $ \conn -> do
  res <- quickQuery' conn query [toSql email, toSql encPass]
  return . fmap toUser . listToMaybe $ res
  where
    query = "SELECT * FROM users WHERE email = ? AND password = ?"
    encPass = sha512 pass
