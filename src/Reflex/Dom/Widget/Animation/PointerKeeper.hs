{-# LANGUAGE Rank2Types, DataKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reflex.Dom.Widget.Animation.PointerKeeper
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Reflex.Dom.Widget.Animation.PointerKeeper
  ( PointerKeeper, newPointerKeeper, listenToWheel
  , altKey, ctrlKey, metaKey, shiftKey, modKeys, buttons
  , eventPointerKeeper
  , downTime, downPointers, curPointers
  , play, pause, step
  , viewPortSize
  ) where

import Control.Monad (join)
import GHCJS.Foreign.Callback (asyncCallback1, Callback)
import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal, IsJSVal)
import GHCJS.DOM.Types (Element)
import JavaScript.Array (JSArray, toList)


import Reflex.Dom.Widget.Animation.Types

-- | A wrapper around Animation.PointerKeeper JS object
--   implemented in jsbits\/Animation.js
newtype PointerKeeper = PointerKeeper JSVal
instance IsJSVal PointerKeeper


-- | Create a new PointerKeeper.
--   Listens for:
--
--     1. Rendering event (RequestAnimationFrame is resolved)
--     2. All pointer events
--     3. Element resize event
newPointerKeeper :: Element
                 -> (AnimationTime -> IO ()) -- ^ Rendering event
                 -> (PointerEvent 'PointerUp -> IO ())
                 -> (PointerEvent 'PointerClick -> IO ())
                 -> (PointerEvent 'PointerDown -> IO ())
                 -> (PointerEvent 'PointerMove -> IO ())
                 -> (PointerEvent 'PointerCancel -> IO ())
                 -> (ResizeEvent -> IO ())   -- ^ Resising element
                 -> IO PointerKeeper
newPointerKeeper el updateCallback pUpCallback pClickCallback pDownCallback pMoveCallback pCancelCallback resizeCallback = join
     $ js_pointerKeeper el
    <$> asyncCallback1 (updateCallback . pFromJSVal)
    <*> asyncCallback1 (pUpCallback . PointerEvent )
    <*> asyncCallback1 (pClickCallback . PointerEvent)
    <*> asyncCallback1 (pDownCallback . PointerEvent)
    <*> asyncCallback1 (pMoveCallback . PointerEvent)
    <*> asyncCallback1 (pCancelCallback . PointerEvent)
    <*> asyncCallback1 (resizeCallback . ResizeEvent . js_unpackCoords2D)

foreign import javascript unsafe "$r = new Animation.PointerKeeper($1,$2,$3,$4,$5,$6,$7,$8);"
   js_pointerKeeper :: Element
      -> Callback (JSVal -> IO ())
      -> Callback (JSVal -> IO ())
      -> Callback (JSVal -> IO ())
      -> Callback (JSVal -> IO ())
      -> Callback (JSVal -> IO ())
      -> Callback (JSVal -> IO ())
      -> Callback (JSVal -> IO ())
      -> IO PointerKeeper



foreign import javascript unsafe "$1.play()" play :: PointerKeeper -> IO ()
foreign import javascript unsafe "$1.playStep()" step :: PointerKeeper -> IO ()
foreign import javascript unsafe "$1.stop()" pause :: PointerKeeper -> IO ()
foreign import javascript unsafe "$1.altKey"   altKey   :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.ctrlKey"  ctrlKey  :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.metaKey"  metaKey  :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.shiftKey" shiftKey :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1['buttons']"  buttons  :: PointerKeeper -> IO Int
foreign import javascript unsafe "$1.downTime" downTime :: PointerKeeper -> IO AnimationTime
viewPortSize :: PointerKeeper -> IO (Double,Double)
viewPortSize = fmap js_unpackCoords2D . js_viewPortSize
foreign import javascript unsafe "[$1.width, $1.height]" js_viewPortSize :: PointerKeeper -> IO JSVal
downPointers :: PointerKeeper -> IO [(Double,Double)]
downPointers = fmap (fmap js_unpackCoords2D . toList) . js_downPointers
foreign import javascript unsafe "$1.downPointers"
    js_downPointers :: PointerKeeper -> IO JSArray
curPointers :: PointerKeeper -> IO [(Double,Double)]
curPointers = fmap (fmap js_unpackCoords2D . toList) . js_curPointers
foreign import javascript unsafe "$1.curPointers"
    js_curPointers :: PointerKeeper -> IO JSArray



-- | List event's active modifier keys
{-# INLINE modKeys #-}
modKeys :: PointerKeeper -> IO [ModKey]
modKeys ev = f metaKey Meta []
         >>= f altKey Alt
         >>= f ctrlKey Ctrl
         >>= f shiftKey Shift
  where
    f m val s = (\r -> if r then val:s else s) <$> m ev

foreign import javascript unsafe "$1.target.pointerKeeper"
  eventPointerKeeper :: PointerEvent t -> PointerKeeper


foreign import javascript unsafe "$1.listenToWheel($2)"
   js_listenToWheel :: PointerKeeper -> Callback (JSVal -> IO ())  -> IO ()
listenToWheel :: PointerKeeper -> (WheelEvent -> IO ()) -> IO ()
listenToWheel el fwheel = asyncCallback1 (fwheel . pFromJSVal) >>= js_listenToWheel el



-- | Get pointer coordinates
foreign import javascript unsafe "$r1=$1[0];$r2=$1[1];" js_unpackCoords2D :: JSVal -> (Double, Double)
