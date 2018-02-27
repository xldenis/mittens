{-# LANGUAGE OverloadedStrings   #-}
module Logger
( runTimedFastLoggerLoggingT
, newTimeCache
, simpleTimeFormat
, withTimedFastLogger
, LogType(..)
, defaultBufSize
) where

import           System.Log.FastLogger
import           System.Log.FastLogger.Date
import           System.Log.FastLogger.File
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import           Control.Monad.Logger
import           Control.Monad.IO.Class

{-
  taken from https://github.com/kazu-yamamoto/logger/pull/87

  once the PR is merged this module will be entirely removed
-}

fileLocStr :: Loc -> String
fileLocStr loc = (loc_package loc) ++ ':' : (loc_module loc) ++
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
  line = show . fst . loc_start
  char = show . snd . loc_start

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False

defaultTimedLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> FormattedTime
              -> LogStr
defaultTimedLogStr loc src level msg time =
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend` "[" `mappend` toLogStr time `mappend` "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack (fileLocStr loc)) `mappend`
            ")\n")

runTimedFastLoggerLoggingT :: MonadIO m => TimedFastLogger -> LoggingT m a -> m a
runTimedFastLoggerLoggingT tfl m = runLoggingT m $ \a b c d -> tfl (defaultTimedLogStr a b c d)
