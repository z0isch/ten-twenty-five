{-# LANGUAGE FlexibleContexts #-}

module Storage where

import Reflex
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import GHCJS.DOM.Storage
import Data.Maybe
import Game

sampleSavedGames storage _ = do
    s <- sample $ pull $ liftIO $ getSavedGames storage
    return (Just s)

getSavedGames storage = do
    existingSavesJSON <- storageGetItem storage "gameSaves"
    return (fromMaybe [] $ decode $ C.pack existingSavesJSON :: [GameSave])

saveGame storage gs = do
    existingSaves <- getSavedGames storage
    storageSetItem storage "gameSaves" $ C.unpack $ encode $ existingSaves ++ [gs]

clearSavedGames storage = storageSetItem storage "gameSaves" ""
