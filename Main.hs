{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Data.Time.Clock.POSIX
import Data.Monoid
import qualified Data.List as L
import qualified Data.Map.Lazy as Map
import GHCJS.DOM.Storage
import GHCJS.DOM
import GHCJS.DOM.DOMWindow
import Data.Maybe
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C
import Game
import Router
import Storage
import JSBits

main :: IO ()
main = do
    window <- fromJust <$> currentWindow
    storage <- fromJust <$> domWindowGetLocalStorage window
    mainWidgetWithHead siteCss $ container $ do
      rec hR <- router homeView True [gotoH,gGotoH,gSaved] [gotoG,gotoHist]
          histR <- router (historyView storage saveChanged) False [gotoHist] [gotoH] 
          gR <- router (playGameView storage) False [gotoG] [gSaved, gGotoH]
          gotoG <- mapDyn fst hR
          gotoHist <- mapDyn snd hR
          gCleared <- mapDyn fst histR
          gotoH <- mapDyn snd histR          
          gSaved <- mapDyn fst gR
          gGotoH <- mapDyn snd gR
          saveChangedDyn <- mconcatDyn [gSaved,gCleared]
          let saveChanged = push (sampleSavedGames storage) (switchPromptlyDyn saveChangedDyn)
      return ()

buttonAttr :: MonadWidget t m => Map.Map String String -> String -> m (Event t ())
buttonAttr attrs s = do
  (e, _) <- elAttr' "button" attrs $ text s
  return $ domEvent Click e

container = elClass "div" "container"
row = elClass "div" "row"
col size i = elClass "div" ("col-"++size++"-"++(show i))
thRowScope = elAttr "th" (Map.fromList ([("scope","row")]))
siteCss = do
  elAttr "link" (cssMap "bootstrap.min.css") $ return ()
  elAttr "link" (cssMap "chartist.min.css") $ return ()
  elAttr "link" (cssMap "site.css") $ return ()
    where
        cssMap loc = Map.fromList ([("href",loc),("type","text/css"),("rel","stylesheet")])

areYouSureModal (btnAttrs,btnTxt) yesTxt noTxt body = do
  modalID <- liftIO $ getNewModalID
  _ <- buttonAttr (Map.insert "data-toggle" "modal" (Map.insert "data-target" ("#"++modalID) btnAttrs)) btnTxt
  elAttr "div" (Map.fromList [("class","modal fade"),("id",modalID),("data-backdrop","static")]) $ do
          elAttr "div" (Map.fromList [("class","modal-dialog")]) $ do
            elAttr "div" (Map.fromList [("class","modal-content")]) $ do
              elAttr "div" (Map.fromList [("class","modal-body")]) body
              elAttr "div" (Map.fromList [("class","modal-footer")]) $ do
                yesBtn <- buttonAttr (Map.fromList [("class","btn btn-primary"),("data-dismiss","modal")]) yesTxt
                noBtn <- buttonAttr (Map.fromList [("class","btn btn-default"),("data-dismiss","modal")]) noTxt
                return (yesBtn, noBtn)

getNewModalID :: IO (String)
getNewModalID = getCurrentTime >>= return . (map repl) . show . fromRational .  toRational . utcTimeToPOSIXSeconds
  where
    repl '.' = '-'
    repl c = c

homeView = do
  btns <- row $ elClass "div" "col-sm-13" $ do
    elClass "div" "jumbotron" $ do
      elClass "h1" "center-text" $ text "1025"
    playGameBtn <- buttonAttr ("class" =: "btn btn-primary btn-lg btn-block") "Start new game"
    historyBtn <- buttonAttr ("class" =: "btn btn-primary btn-lg btn-block") "View History"
    return (playGameBtn, historyBtn)
  elClass "div" "navbar-fixed-bottom" $ container $ 
    elClass "p" "center-text" $ text "Powered by Haskell"
  return btns
    
historyView storage saveChanged = mdo
  homeBtn <- row $ buttonAttr ("class" =: "btn btn-primary btn-block") "Home"
  savedGames <- liftIO $ getSavedGames storage
  gameHistoryDyn <- holdDyn savedGames saveChanged
  numGamesDyn <- mapDyn length gameHistoryDyn
  bestScoreDyn <- mapDyn bestScore gameHistoryDyn
  avgScoreDyn <- mapDyn (trimTo 2 . averageScore) gameHistoryDyn
  rPercentDyn <- mapDyn getAvgRoundPercents gameHistoryDyn
  row $ do
    elClass "div" "col-xs-12" $ elClass "div" "page-header" $ do
      elClass "div" "h3" $ do
        display numGamesDyn
        text " Games Played"
      scorePanel "Best" bestScoreDyn
      scorePanel "Average" avgScoreDyn
    elClass "div" "col-xs-12" $ elClass "table" "table" $ do
      el "thead" $ el "tr" $ do
        el "th" $ text "Distance"
        el "th" $ text "Make Percentage"
      el "tbody" $ simpleList rPercentDyn roundRow
  elAttr "div" (Map.fromList [("class","ct-chart ct-perfect-fourth"),("id","historyChart")]) $ return ()
  pb <- getPostBuild 
  performEvent_ $ ffor pb $ (\_ -> do
        liftIO $ makeChart "historyChart" savedGames
        return ()
    )
  performEvent_ $ ffor saveChanged $ liftIO .  (makeChart "historyChart")
  (clearBtn,_) <- areYouSureModal (Map.fromList [("class","btn btn-danger btn-block")],"Clear Game History") "Yep" "Nope" $ text "You really want to delete your game history?"
  clear <- performEvent $ ffor clearBtn $ const $ liftIO $ clearSavedGames storage
  return (clear,homeBtn)
  where
    scorePanel t dyn = do
        elClass "div" "col-xs-6" $ do
            elClass "div" "panel panel-default" $ do
                elClass "div" "panel-heading" $ text t
                elClass "div" "panel-body" $ display dyn

    roundRow rpDyn = do
        d <- mapDyn (\(RoundPercent _ dist) -> show dist) rpDyn
        p <- mapDyn (\(RoundPercent perc _) -> show $ trimTo 2 perc) rpDyn
        el "tr" $ do
           el "td" $ dynText d
           el "td" $ dynText p

    trimTo :: (RealFrac r) => Int -> r -> Double
    trimTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

playGameView storage = do
  elClass "div" "row" $ elClass "div" "col-xs-12" $ do
    rec homeBtn <- elClass "div" "row" $ buttonAttr ("class" =: "btn btn-primary btn-block") "Home"
        game <- gameTable saveTime
        (saveBtn,_) <- areYouSureModal (Map.fromList [("class","btn btn-success btn-block")],"End Game") "Save" "Cancel" $ do
          scoreDyn <- mapDyn scoreGame game
          el "h3" $ do
            text "Your score was: "
            display scoreDyn
        saveTime <- performEvent $ ffor saveBtn (\_ -> do
            t <- liftIO getCurrentTime
            g <- sample $ current game
            liftIO $ saveGame storage (GameSave t g)
            return ()
            )
    return (saveTime,homeBtn)
  where
    gameTable saveBtn = elClass "table" "table table-responsive" $ do
        el "thead" $ el "tr" $ do
            el "th" $ text "Distance"
            replicateM_ 6 (el "th" $ text "")
            el "th" $ text "Score"
        el "tbody" $ do
            rounds <- zipWithM ($) (replicate 6 (roundCheckboxRows saveBtn)) [10,15,20,25,30,35]
            srounds <- mapM (mapDyn (\s -> [s])) rounds
            gameDyn <- mconcatDyn srounds
            score <- mapDyn scoreGame gameDyn
            elClass "tr" "active" $ do
                thRowScope $ text "Total"
                replicateM_ 6 $ el "td" $ text ""
                el "td"  $ display score
            return gameDyn

    roundCheckboxRows saveBtn dist = el "tr" $ do
        thRowScope $ text $ (show dist)++"'"
        cs <- replicateM 6 $ (el "td" $ checkbox False $ def & checkboxConfig_setValue .~ (fmap (const False) saveBtn))
        let vs = map _checkbox_value cs
        vls <- mapM (mapDyn (\t -> [t])) vs
        combined <- mconcatDyn vls
        round <- mapDyn (\rs -> Round rs dist) combined
        score <- mapDyn scoreRound round
        el "td" $ display score
        return round
