module Router where

import Reflex.Dom
import Control.Monad.IO.Class
import Data.Monoid
import Data.Semigroup

router
  :: (Monoid b, MonadWidget t m, Semigroup b1, Semigroup b2) =>
     m b
     -> Bool
     -> [Dynamic t (Event t b1)]
     -> [Dynamic t (Event t b2)]
     -> m (Dynamic t b)
router widget initVisibility inEvents outEvents  = do
    inEvent <- squash True inEvents
    outEvent <- squash False outEvents
    widgetHoldHelper (visWidget widget) initVisibility $ leftmost [inEvent,outEvent] 
        
squash
  :: (Reflex t, MonadHold t f, Semigroup b1) =>
     b -> [Dynamic t (Event t b1)] -> f (Event t b)
squash val events = fmap (const val) <$> switchPromptlyDyn <$> mconcatDyn events

visWidget :: (Monad m, Monoid a) => m a -> Bool -> m a
visWidget w True = w
visWidget _ False = return mempty

widgetHoldHelper
    :: MonadWidget t m
    => (a -> m b)
    -> a
    -> Event t a
    -> m (Dynamic t b)
widgetHoldHelper f eDef e = widgetHold (f eDef) (f <$> e)
