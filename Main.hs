{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom
import Reflex.Spider.Internal (SpiderHostFrame)
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
import qualified GHCJS.DOM.Types as GT
import GHCJS.DOM.EventM
import GHCJS.DOM.EventTargetClosures
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
    mainWidgetWithHead siteCss $ do
      doc <- askDocument
      dR <- deviceReady 
      _ <- widgetHoldHelper (visWidget $ mainWidget storage) False dR
      return ()
    where 
        mainWidget storage = do
          navHomeClick <- nav
          container $ mdo 
            let navHomeBtn = constDyn navHomeClick
            backButtonE <- constDyn <$> backButton
            hR <- router homeView True [gSaved,backButtonE,navHomeBtn] [gotoG,gotoHist]
            gCleared <- router (historyView storage saveChanged) False [gotoHist] [backButtonE, navHomeBtn] 
            gSaved <- router (playGameView storage)  False [gotoG] [gSaved, backButtonE, navHomeBtn]
            gotoG <- mapDyn fst hR
            gotoHist <- mapDyn snd hR
            saveChangedDyn <- mconcatDyn [gSaved,gCleared]
            let saveChanged = push (sampleSavedGames storage) (switchPromptlyDyn saveChangedDyn)
            return ()
          return ()

backButton :: Widget
       Spider
       (Gui
          Spider
          (WithWebView SpiderHost)
          SpiderHostFrame)
       (Event Spider ())
backButton = do
    doc <- askDocument
    wrapDomEvent doc (connectEvent "backbutton") (return ())

deviceReady :: Widget
       Spider
       (Gui
          Spider
          (WithWebView SpiderHost)
          SpiderHostFrame)
       (Event Spider Bool) 
deviceReady 
    | cordova = do
        doc <-askDocument
        wrapDomEvent doc (connectEvent "deviceReady") (return True)
    | otherwise = do
        pb <- getPostBuild
        return $ fmap (const True) pb

connectEvent :: (GT.GObjectClass s) => String -> s -> EventM GT.Event s () -> IO (IO ())
connectEvent = connect 

buttonAttr :: MonadWidget t m => Map.Map String String -> String -> m (Event t ())
buttonAttr attrs s = do
  (e, _) <- elAttr' "button" attrs $ text s
  return $ domEvent Click e

container = elClass "div" "container"
row = elClass "div" "row"
col size i = elClass "div" ("col-"++size++"-"++show i)
thRowScope = elAttr "th" (Map.fromList [("scope","row")])
siteCss = do
  elAttr "link" (cssMap "bootstrap.min.css") $ return ()
  elAttr "link" (cssMap "chartist.min.css") $ return ()
  elAttr "link" (cssMap "site.css") $ return ()
    where
        cssMap loc = Map.fromList [("href",loc),("type","text/css"),("rel","stylesheet")]

    
nav = do
    elClass "nav" "navbar navbar-default" $ do
        divClass "container-fluid" $ do
            divClass "navbar-header" $ do
                (e,_) <- elAttr' "a" (Map.fromList [("class", "navbar-brand"),("href","#")]) $ do
                    elAttr "img" (Map.fromList [("id","navbar-brand"),("src","Driven-Web-Logo.png")]) $ return ()       
                return $ domEvent Click e

areYouSureModal (btnAttrs,btnTxt) yesTxt noTxt body = do
  modalID <- liftIO getNewModalID
  _ <- buttonAttr (Map.insert "data-toggle" "modal" (Map.insert "data-target" ("#"++modalID) btnAttrs)) btnTxt
  elAttr "div" (Map.fromList [("class","modal fade"),("id",modalID),("data-backdrop","static")]) $
          elAttr "div" (Map.fromList [("class","modal-dialog")]) $
            elAttr "div" (Map.fromList [("class","modal-content")]) $ do
              elAttr "div" (Map.fromList [("class","modal-body")]) body
              elAttr "div" (Map.fromList [("class","modal-footer")]) $ do
                yesBtn <- buttonAttr (Map.fromList [("class","btn btn-primary"),("data-dismiss","modal")]) yesTxt
                noBtn <- buttonAttr (Map.fromList [("class","btn btn-default"),("data-dismiss","modal")]) noTxt
                return (yesBtn, noBtn)

getNewModalID :: IO String
getNewModalID = liftM (map repl . show . fromRational .  toRational . utcTimeToPOSIXSeconds) getCurrentTime 
  where
    repl '.' = '-'
    repl c = c

homeView = do
  row $ elClass "div" "col-sm-12" $ do
    elAttr "img" (Map.fromList [("class","img-responsive"),("src","1025-big.png")]) $ return ()
  el "br" $ return ()
  btns <- row $ elClass "div" "col-sm-12" $ do
    playGameBtn <- buttonAttr ("class" =: "btn btn-primary btn-lg btn-block") "Play Game"
    historyBtn <- buttonAttr ("class" =: "btn btn-primary btn-lg btn-block") "View History"
    return (playGameBtn, historyBtn)
  elClass "div" "navbar-fixed-bottom" $ container $ 
    elClass "p" "center-text" $ text "Powered by Haskell"
  return btns
    
historyView storage saveChanged = mdo
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
  row $ elClass "div" "col-xs-12" $ elClass "table" "table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ text "Distance"
      el "th" $ text "Make Percentage"
    el "tbody" $ simpleList rPercentDyn roundRow
  elAttr "div" (Map.fromList [("class","ct-chart ct-perfect-fourth"),("id","historyChart")]) $ return ()
  pb <- getPostBuild 
  performEvent_ $ ffor pb (\_ -> do
        liftIO $ makeChart "historyChart" savedGames
        return ()
    )
  performEvent_ $ ffor saveChanged $ liftIO .  makeChart "historyChart"
  (clearBtn,_) <- areYouSureModal (Map.fromList [("class","btn btn-danger btn-block")],"Clear Game History") "Yep" "Nope" $ text "You really want to delete your game history?"
  clear <- performEvent $ ffor clearBtn $ const $ liftIO $ clearSavedGames storage
  return (clear)
  where
    scorePanel t dyn =
        elClass "div" "col-xs-6" $
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
    trimTo n f = fromInteger $ round $ f * (10^n) / (10.0^^n)

playGameView storage =
  elClass "div" "row" $ elClass "div" "col-xs-12" $ do
    rec game <- gameTable saveTime
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
    return (saveTime)
  where
    gameTable saveBtn = elClass "table" "table table-responsive" $ do
        el "thead" $ el "tr" $ do
            el "th" $ text "Distance"
            replicateM_ 6 (el "th" $ text "")
            el "th" $ text "Score"
        el "tbody" $ do
            rounds <- zipWithM ($) (replicate 6 (roundCheckboxRows saveBtn)) [10,15,20,25,30,35]
            srounds <- mapM (mapDyn (: [])) rounds
            gameDyn <- mconcatDyn srounds
            score <- mapDyn scoreGame gameDyn
            elClass "tr" "active" $ do
                thRowScope $ text "Total"
                replicateM_ 6 $ el "td" $ text ""
                el "td"  $ display score
            return gameDyn

    roundCheckboxRows saveBtn dist = el "tr" $ do
        thRowScope $ text $ show dist  ++ "'"
        cs <- replicateM 6 $ el "td" $ checkbox False $ def & checkboxConfig_setValue .~ fmap (const False) saveBtn
        let vs = map _checkbox_value cs
        vls <- mapM (mapDyn (: [])) vs
        combined <- mconcatDyn vls
        round <- mapDyn (`Round` dist) combined
        score <- mapDyn scoreRound round
        el "td" $ display score
        return round
