{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Maybe
import           Data.Pool
import           Data.String                 (fromString)
import           Data.Word

import           Control.Concurrent.Async
import           Control.Exception

import qualified Database.MySQL.Base         as Base
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Types

import           Control.Concurrent
import           Data.Monoid
import           Data.Time.Clock

import           Control.Monad
import           Options.Applicative.Simple
import           Data.List
import           Data.Char

data CLI = CLI
  { host     :: String
  , port     :: Word16
  , user     :: String
  , password :: String
  , database :: String
  }

cliParser prefix = CLI
  <$> strOption   (long (prefix ++ "host"))
  <*> option auto (long (prefix ++ "port") <> value 3306)
  <*> strOption   (long (prefix ++ "user"))
  <*> strOption   (long (prefix ++ "password") <> value "")
  <*> strOption   (long (prefix ++ "database") <> value "")

main = do
  (sourceDB, targetDB) <- execParser (info ((,) <$> cliParser "source-" <*> cliParser "dest-") fullDesc)

  sourcePool <- createPool (connect $ mkConnectInfo sourceDB) close 1 2 1
  targetPool <- createPool (connect $ mkConnectInfo targetDB) close 1 15 10

  repeatingEvery 1 $ do
    queries <- withResource sourcePool (getQueries $ database sourceDB)
    putStrLn $ "Fetched " ++ show (length queries) ++ " queries from " ++ host sourceDB ++ " db " ++ database sourceDB
    mapConcurrently_ (\q -> withResource targetPool $ \conn -> runQuery conn q) queries

getQueries :: String -> Connection -> IO [Query]
getQueries dbName conn = do
  (results :: [Only (Maybe String)]) <- query conn
    "select sql_text \
    \from performance_schema.events_statements_history as esh \
    \join performance_schema.threads on threads.thread_id = esh.thread_id \
    \where type='foreground' and current_schema = ? and processlist_id != connection_id() \
    \and event_name='statement/sql/select';"
    (Only dbName)

  let queries = mapMaybe fromOnly results
      readQueries = filter isReadQuery queries

  return $ map fromString readQueries

-- | Shitty filter to identify read only queries, for now assume all selects are read-only.
isReadQuery :: String -> Bool
isReadQuery str =
  isPrefixOf "select" (map toLower str) &&
  not (isSuffixOf "..." str)

mkConnectInfo :: CLI -> ConnectInfo
mkConnectInfo cli =
  defaultConnectInfo
    { connectHost = host cli
    , connectPort = port cli
    , connectUser = user cli
    , connectPassword = password cli
    , connectDatabase = database cli
    }

runQuery :: Connection -> Query -> IO ()
runQuery conn text =
  doQuery `catches`
    [ Handler $ \(_ :: FormatError) -> logError
    , Handler $ \(_ :: QueryError)  -> logError
    , Handler $ \(_ :: ResultError) -> logError
    , Handler $ \(e :: Base.MySQLError)  -> putStrLn $ "Hit an invalid query: " <> show (fromQuery text) <> show e
    ]
  where
  logError = putStrLn "omg an error happened!!!"
  doQuery = do
    Base.query conn (fromQuery text)

    result <- Base.storeResult conn
    Base.freeResult result

    return ()

repeatingEvery :: NominalDiffTime -> IO () -> IO ()
repeatingEvery time action = do
  current <- getCurrentTime
  action
  updated <- getCurrentTime

  let diff = diffUTCTime updated current

  putStrLn $ "Queries were replicated in " ++ show diff ++ " seconds."

  when (time - diff > 0) $ threadDelay (floor $ 10^6 * (time - diff))

  repeatingEvery time action
