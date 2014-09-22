{-# LANGUAGE OverloadedStrings #-}

module Pit (
  get,
  set,
  switch
  ) where

import Control.Applicative ((<$>))
import Control.Monad (unless)

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
pitDirectory = (F.</> ".pit") <$> getHomeDirectory

pitConfigFile :: IO FilePath
pitConfigFile = (F.</> "pit.yaml") <$> pitDirectory

pitProfileFile :: FilePath -> IO FilePath
pitProfileFile profile =
  (\dir -> dir F.</> profile F.<.> "yaml") <$> pitDirectory

writeDefaultConfig :: IO ()
writeDefaultConfig = switch "default"

loadProfile :: Text -> IO (Maybe Config)
loadProfile profile' = do
  let profile = T.unpack profile'
  file <- pitProfileFile profile
  exist <- doesFileExist file
  if exist then Y.decodeFile file else return Nothing

getProfile :: IO Text
getProfile = do
  file <- pitConfigFile
  conf <- fromJust <$> Y.decodeFile file
  return . fromJust $ H.lookup ("profile" :: Text) conf

-- If '~/.pit' directory or 'pit.yaml' file don't exist, make them.
initialize :: IO ()
initialize = do
  dir <- pitDirectory
  createDirectoryIfMissing False dir
  existsConf <- pitConfigFile >>= doesFileExist
  unless existsConf writeDefaultConfig

get :: (Y.FromJSON a) => Text -> IO (Maybe a)
get name = do
  initialize
  prof <- getProfile
  conf <- loadProfile prof
  case conf of
   Nothing -> return Nothing
   Just c -> case H.lookup name c of
     Nothing -> return Nothing
     Just v -> return $ Y.parseMaybe Y.parseJSON v

set :: (Y.ToJSON a) => Text -> a -> IO ()
set name value = do
  initialize
  prof <- getProfile
  conf <- fromMaybe H.empty <$> loadProfile prof
  let newConf = H.insert name (Y.toJSON value) conf
  file <- pitProfileFile $ T.unpack prof
  Y.encodeFile file newConf

switch :: Text -> IO ()
switch newProf = do
  let newConf = Y.object ["profile" Y..= Y.String newProf]
  file <- pitConfigFile
  Y.encodeFile file newConf
