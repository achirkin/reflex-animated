{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as Text
import qualified Data.Map.Lazy as Map

import Reflex.Dom
import Reflex.Dom.Widget.Animation as Animation
import JavaScript.Web.Canvas


main :: IO ()
main = do
    mainWidget $ el "div" $ do
      canvas <- fst <$> elAttr' "canvas"
                        ( Map.fromList [("width", "600")
                                       ,("height", "250")
                                       ,("style"
                                        , Text.unwords
                                              [ "margin: 22px;"
                                              , "padding: 10px 20px 30px 25px;"
                                              , "border-color: darkred;"
                                              , "border-style: dashed;"
                                              , "background-color: white;"
                                              , "width: 65%;"
                                              , "height: 400px;"]
                                        )]
                        ) (pure ())

      el "p" $ text "Reflex is:"
      el "ul" $ do
        el "li" $ text "Efficient"
        el "li" $ text "Higher-order"
        el "li" $ text "Glitch-free"

      ctx <- liftIO $ getElementContext canvas

      -- initialize animation handler (and all pointer events).
      aHandler <- Animation.registerHandler (_element_raw canvas) Nothing -- (Just print)

      -- draw a square every time I click on canvas
      performEvent_ . ffor (select (pointerEvents aHandler) PClickEvent) $ \ev -> liftIO $ do
        fillStyle 0x00 0xAA 0x66 0.4 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-4) (y-4) 9 9 ctx

      -- another square on pointer up event
      performEvent_ . ffor (select (pointerEvents aHandler) PUpEvent) $ \ev -> liftIO $ do
        fillStyle 0x00 0xFF 0xFF 0.9 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-2) (y-2) 5 5 ctx

      -- Combine event with a behavior "is shift button pressed when pointer down event fires?".
      -- Conditionally on shift button pressed I draw rectangle in different styles.
      performEvent_ . ffor ((,) <$> shiftKeyB aHandler <@> select (pointerEvents aHandler) PDownEvent) $ \( shift, ev) -> liftIO $ do
        if shift then fillStyle 0x00 0x11 0x00 1.0 ctx
                 else fillStyle 0xFF 0xFF 0xAA 1.0 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-7) (y-7) 15 15 ctx

      -- Conditionally on ctrl button pressed, draw rectangle in different styles on mouse move events
      performEvent_ . ffor ((,) <$> ctrlKeyB aHandler <@> select (pointerEvents aHandler) PMoveEvent)
                    $ \(ctrl, ev) -> liftIO $ do
        if ctrl then fillStyle 0x55 0xFF 0x99 1.0 ctx
                else fillStyle 0x00 0x33 0xAA 0.3 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-3) (y-3) 7 7 ctx

      -- Pointer cancel event usually happens when mouse gets out of the element area
      performEvent_ . ffor (select (pointerEvents aHandler) PCancelEvent) $ \ev -> liftIO $ do
        fillStyle 0xFF 0x00 0x00 1.0 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-12) (y-12) 25 25 ctx

      -- Clear screen based on mouse wheel event.
      -- Use current viewPortSize to find out dimensions of the element.
      performEvent_ . ffor ((,) <$> current (viewPortSizeD aHandler) <@> wheelEvents aHandler) $ \((w,h), ev) -> liftIO $ do
        case ev of
          WheelUp   -> fillStyle 0xFF 0xCC 0xCC 1.0 ctx
          WheelDown -> fillStyle 0xCC 0xCC 0xFF 1.0 ctx
        fillRect 0 0 w h ctx





getElementContext :: Element r s t -> IO Context
getElementContext = getContext . unsafeCoerce . _element_raw
