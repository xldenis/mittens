{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Data.Pool
import Data.Text (Text)
import Data.Maybe
import Data.String (fromString)
import Data.Word
import Data.Maybe

import Control.Exception
import Control.Concurrent.Async

import qualified Database.MySQL.Base as Base
import Database.MySQL.Simple
import Database.MySQL.Simple.Types

import Data.Monoid
import Data.Time.Clock
import Control.Concurrent

import Options.Applicative.Simple
import Control.Monad
type SQLText = Text

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
  targetPool <- createPool (connect $ mkConnectInfo targetDB) close 1 2 5

  repeatingEvery 5 $ do
    queries <- withResource sourcePool getQueries

    print queries
    mapConcurrently_ (\q -> withResource targetPool $ \conn -> runQuery conn q) queries

getQueries :: Connection -> IO [Query]
getQueries conn  = do
  (results :: [Only (Maybe String)]) <- query_ conn "select sql_text from performance_schema.events_statements_history"
  return $ map fromString $ catMaybes $ map fromOnly results

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
  doQuery `catch` \(_ :: FormatError) -> logError
          `catch` \(_ :: QueryError)  -> logError
          `catch` \(_ :: ResultError) -> logError

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

  when (time - diff > 0) $ threadDelay (floor $ 10^6 * (time - diff))

  repeatingEvery time action
