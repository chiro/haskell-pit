{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as H
import Data.Text (Text())
import qualified Data.Text as T
import qualified Data.Yaml as Y

import Options.Applicative

import Pit

type HM = H.HashMap Text Text

-- | Text 'Option' Reader
text :: Monad m => String -> m Text
text = return . T.pack

opts :: Parser (IO ())
opts = subparser (
  command "get" (info (getCommand <$> argument text idm) idm)
  <> command "set" (info (setCommand <$> argument text idm) idm)
  <> command "switch" (info (Pit.switch <$> argument text idm) idm)
  )

printYaml :: Y.Value -> IO ()
printYaml yaml = putStrLn $ C.unpack $ Y.encode yaml

getCommand :: Text -> IO ()
getCommand key = do
  v <- Pit.get key Y.Null
  printYaml v

setCommand :: Text -> IO ()
setCommand key = do
  Pit.set key
  v <- Pit.get key Y.Null
  printYaml v

main :: IO ()
main = join $ execParser (info opts idm)
