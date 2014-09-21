{-# LANGUAGE OverloadedStrings #-}

module Pit where

import Control.Monad (when)

import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Yaml as Y

import System.Directory
import qualified System.FilePath as F

import Data.Text (Text())
import qualified Data.Text as T

type Config = HashMap Text Y.Value

pitDirectory :: IO FilePath
pitDirectory = do
  homeDir <- getHomeDirectory
  return $ homeDir F.</> ".pit"

pitConfigFile :: IO FilePath
pitConfigFile = do
  pitDir <- pitDirectory
  return $ pitDir F.</> "pit.yaml"

pitProfileFile :: FilePath -> IO FilePath
pitProfileFile profile = do
  pitDir <- pitDirectory
  return $ pitDir F.</> (profile F.<.> ".yaml")

writeDefaultConfig :: IO ()
writeDefaultConfig = changeProfile "default"

loadProfile :: FilePath -> IO (Maybe Config)
loadProfile profile = do
  file <- pitProfileFile profile
  exist <- doesFileExist file
  if exist then Y.decodeFile file else return Nothing

getProfile :: IO Text
getProfile = do
  file <- pitConfigFile
  conf <- fmap fromJust $ Y.decodeFile file
  return . fromJust $ H.lookup ("profile" :: Text) conf

changeProfile :: Text -> IO ()
changeProfile new = do
  let newConf = Y.object ["profile" Y..= Y.String new]
  file <- pitConfigFile
  Y.encodeFile file newConf

loadConfig :: IO (Maybe a)
loadConfig = do
  dir <- pitDirectory
  conf <- pitConfigFile
  createDirectoryIfMissing False dir
  existsConf <- doesFileExist conf
  when (not existsConf) writeDefaultConfig
  return Nothing

get :: (Y.FromJSON a) => Text -> IO (Maybe a)
get name = do
  prof <- getProfile
  conf <- loadProfile $ T.unpack prof
  case conf of
   Nothing -> return Nothing
   Just c -> case H.lookup name c of
     Nothing -> return Nothing
     Just v -> return $ Y.parseMaybe Y.parseJSON v

set :: (Y.ToJSON a) => Text -> a -> IO ()
set name value = do
  prof <- getProfile
  conf <- fmap (fromMaybe H.empty) $ loadProfile $ T.unpack prof
  let newConf = H.insert name (Y.toJSON value) conf
  file <- pitProfileFile $ T.unpack prof
  Y.encodeFile file newConf
