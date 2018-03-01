{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Pool

import           UnliftIO.Async

import           Control.Concurrent
import           Data.Monoid
import           Data.Time.Clock

import           Control.Monad
import           Options.Applicative.Simple

import           Control.Monad.IO.Unlift

globalOpts = (,,)
  <$> option auto (long "spacing" <> value 1)
  <*> option auto (long "count"   <> value 10)
  <*> option auto (long "resources" <> value 50)

main = do
  (spacing, count, resources) <- execParser $ info (globalOpts) fullDesc

  pool <- createPool (pure ()) (const $ pure ()) 1 5 resources

  let list = replicate count spacing

  time <- elapsed $
    mapConcurrently_ (\i -> withResource pool $ \_ -> threadDelay i) list

  capabilities <- getNumCapabilities
  print $ "Took " <> show time <> " using " <> show capabilities <> " capabilities."

elapsed :: IO a -> IO NominalDiffTime
elapsed action = do
  current <- getCurrentTime
  action
  updated <- getCurrentTime

  return $ diffUTCTime updated current
