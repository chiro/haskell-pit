{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join, when)

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as H
import Data.Maybe (isJust, fromJust)
import Data.Text (Text())
import qualified Data.Text as T
import qualified Data.Yaml as Y

import Options.Applicative

import System.Process
import System.Environment
import System.IO
import System.IO.Temp

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

openEditorAndGetNewValue :: Maybe Y.Value -> IO (Maybe Y.Value)
openEditorAndGetNewValue def = do
  editor' <- lookupEnv "EDITOR"
  tty <- hIsTerminalDevice stdout
  if isJust editor' && tty
    then withSystemTempFile "new.yaml" $ \path h -> do
    hClose h
    when (isJust def) $ do
      let content = C.unpack $ Y.encode $ fromJust def
      writeFile path content
    _ <- callCommand (fromJust editor' ++ " " ++ path)
    Y.decodeFile path
    else return Nothing

getCommand :: Text -> IO ()
getCommand key = do
  v <- Pit.get key :: IO (Maybe Y.Value)
  case v of
   Nothing -> do
     v' <- openEditorAndGetNewValue Nothing
     case v' of
      Nothing -> putStrLn "Failed to get the value."
      Just v'' -> do
        Pit.set key v''
        printYaml v''
   Just v' -> printYaml v'

setCommand :: Text -> IO ()
setCommand key = do
  v <- Pit.get key :: IO (Maybe Y.Value)
  v' <- openEditorAndGetNewValue v
  case v' of
   Nothing -> putStrLn "Failed to set the value."
   Just v'' -> do
     Pit.set key v''
     putStrLn "Succeed to set the value."

main :: IO ()
main = join $ execParser (info opts idm)
