{-# LANGUAGE JavaScriptFFI,CPP #-}

module JSBits where

import Data.Maybe
import Data.Aeson
import Game

#ifdef __GHCJS__
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign

foreign import javascript unsafe "new Chartist.Line($1, $2, {axisX:{showGrid:false,showLabel:false},axisY:{showGrid:false}},{height:300,width:300});"
  gameChart :: JSString -> (JSRef a) -> IO ()

makeChart id games = do
  gChart <- toJSRef_aeson $ getScoreChart games
  gameChart (toJSString $ "#"++id) gChart
#else
gameChart = error "gameChart: only available from JavaScript"
makeChart = error "makeChart: only available from JavaScript"
#endif
