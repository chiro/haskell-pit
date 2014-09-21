{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))

import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)

import Data.Text (Text())

import Pit

type HM = H.HashMap Text Text

main :: IO ()
main = do
  let hm = H.insert ("user" :: Text) ("chiro" :: Text) H.empty
  set "github" hm
  h <- (fromMaybe (H.empty :: HM)) <$> get "github"
  print h
