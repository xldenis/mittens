{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Maybe
import           Data.Pool
import           Data.String                 (fromString)
import           Data.Word

import           UnliftIO.Async
import           UnliftIO.Exception

import qualified Database.MySQL.Base         as Base
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Types

import           Control.Concurrent
import           Data.Monoid
import           Data.Time.Clock

import           Control.Monad
import           Data.Char
-- import           Data.List
import           Options.Applicative.Simple

import           Control.Monad.IO.Class
import           Control.Monad.Logger

import           Control.Monad.IO.Unlift
import           Logger
import Data.ByteString (ByteString, isSuffixOf)
import Data.Coerce

data DBOpts = DB
  { host     :: String
  , port     :: Word16
  , user     :: String
  , password :: String
  , database :: String
  }

data Global = Opts
  { interval :: Word32
  , threads  :: Word32
  }

cliParser prefix = DB
  <$> strOption   (long (prefix ++ "host"))
  <*> option auto (long (prefix ++ "port") <> value 3306)
  <*> strOption   (long (prefix ++ "user"))
  <*> strOption   (long (prefix ++ "password") <> value "")
  <*> strOption   (long (prefix ++ "database") <> value "")

globalOpts = Opts
  <$> option auto (long "interval" <> value 1)
  <*> option auto (long "threads"  <> value 10)

main = do
  (sourceDB, targetDB, opts) <- execParser $ info ((,,) <$> cliParser "source-" <*> cliParser "dest-" <*> globalOpts) fullDesc

  sourcePool <- createPool (connect $ mkConnectInfo sourceDB) close 1 2 1
  targetPool <- createPool (connect $ mkConnectInfo targetDB) close 1 15 (fromIntegral $ threads opts)

  times <- newTimeCache simpleTimeFormat
  do
  -- withTimedFastLogger times (LogStdout defaultBufSize) $ \logger ->
    -- runTimedFastLoggerLoggingT logger $
    repeatingEvery (fromIntegral $ interval opts) $
      pushQueryBatchToDest sourcePool targetPool sourceDB

-- pushQueryBatchToDest :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => Pool Connection -> Pool Connection -> DBOpts -> m ()
pushQueryBatchToDest sourcePool targetPool sourceDb = do
  (queries, diff) <- elapsedTime $ withResource sourcePool (getQueries $ database sourceDb)
  -- logInfoN . fromString $ "Fetched " <> show (length queries) <> " queries in " <> fromString (show diff)
  (_, diff) <- elapsedTime $ mapConcurrently_ (\q -> withResource targetPool $ \conn -> runQuery conn q) queries
  -- logInfoN $ "Sent queries to dest in " <> fromString (show diff)
  return ()
getQueries :: MonadIO m => String -> Connection -> m [Query]
getQueries dbName conn = liftIO $ do
  (results :: [Only (Maybe ByteString)]) <- query conn
    "select sql_text from performance_schema.events_statements_history as esh \
    \where current_schema = ? and event_name='statement/sql/select';"
    (Only dbName)

  let queries = mapMaybe fromOnly results
      readQueries = filter isReadQuery queries

  return $ map coerce readQueries

-- | Shitty filter to identify read only queries, for now assume all selects are read-only.
isReadQuery :: ByteString -> Bool
isReadQuery str =
  -- isPrefixOf "select" (map toLower str) &&
  not ("..." `isSuffixOf` str)

mkConnectInfo :: DBOpts -> ConnectInfo
mkConnectInfo cli =
  defaultConnectInfo
    { connectHost = host cli
    , connectPort = port cli
    , connectUser = user cli
    , connectPassword = password cli
    , connectDatabase = database cli
    }

runQuery :: (MonadIO m, MonadUnliftIO m) => Connection -> Query -> m ()
runQuery conn text =
  doQuery `catches`
    [ Handler $ \(e :: FormatError) -> logError (show e)
    , Handler $ \(e :: QueryError)  -> logError (show e)
    , Handler $ \(e :: ResultError) -> logError (show e)
    , Handler $ \(e :: Base.MySQLError)  -> liftIO $ putStrLn . fromString $ "Hit an invalid query: " <> show e
    ]
  where
  logError e = liftIO $ putStrLn . fromString $ "omg an error happened!!! " <> e
  doQuery = liftIO $ do
    Base.query conn (fromQuery text)
    Base.useResult conn >>= Base.freeResult
    return ()

elapsedTime :: MonadIO m => m a -> m (a, NominalDiffTime)
elapsedTime action = do
  current <- liftIO getCurrentTime
  res <- action
  updated <- liftIO getCurrentTime

  return $ (res, diffUTCTime updated current)

repeatingEvery :: (MonadIO m) => NominalDiffTime -> m () -> m ()
repeatingEvery time action = do
  (_, diff) <- elapsedTime action

  liftIO $ putStrLn $ "Queries were replicated in " <> fromString (show diff) <> " seconds."

  when (time - diff > 0) $ liftIO $ threadDelay (floor $ 10^6 * (time - diff))

  repeatingEvery time action
