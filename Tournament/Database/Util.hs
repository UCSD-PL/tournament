module Tournament.Database.Util where

import Database.HDBC
import Database.HDBC.PostgreSQL

database :: String
database = "dbname=tournament"

withDatabase :: (Connection -> IO a) -> IO a
withDatabase f = do conn <- connectPostgreSQL database
                    ret <- f conn
                    commit conn
                    disconnect conn
                    return ret
