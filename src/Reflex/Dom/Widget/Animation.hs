{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}


{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reflex.Dom.Widget.Animation
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Reflex.Dom.Widget.Animation
  ( -- * JS handler
    AnimationHandler (), registerHandler, play, pause
    -- * Events
  , animationEvents, pointerEvents, wheelEvents, resizeEvents, onRenderEvent
    -- * Behaviors
  , altKeyB, ctrlKeyB, metaKeyB, shiftKeyB
  , buttonsB, downPointersB, curPointersB, downTimeB
  , viewPortSizeD, viewPortSizeI
    -- * Data types
  , AnimationTime (..), getAnimationTime
    -- * Events
  , AEventType (..)
  , PEventType (..), asPointerEventType
  , IsPointerEvent (..), eventType
  , PointerEventType (..), PointerUp, PointerDown, PointerMove, PointerClick, PointerCancel
  , PointerEvent (), pointers, eventJSType, button
  , ResizeEvent (..)
  , WheelEvent (..)
    -- * Modifiers
  , ModKey (..)
  ) where


import           Control.Concurrent.MVar    (tryPutMVar, tryTakeMVar, newMVar)
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Data.Coerce                (coerce)
import           Data.IORef                 (newIORef)
import qualified GHCJS.DOM.Types     as DOM (IsElement, toElement)
import           System.IO.Unsafe           (unsafeInterleaveIO)

import           Reflex.Class               (EventSelector (..), Event, Dynamic
                                            ,Behavior (..), Reflex, MonadHold)
import qualified Reflex.Dynamic   as Reflex (holdDyn)
import           Reflex.PerformEvent.Class  (PerformEvent (Performable, performEvent_))
import           Reflex.TriggerEvent.Class  (TriggerEvent (newTriggerEvent))
import qualified Reflex.Spider.Internal as Spider

import           Reflex.Dom.Widget.Animation.Types
import qualified Reflex.Dom.Widget.Animation.PointerKeeper as PK


registerHandler :: forall el t m
                 . ( DOM.IsElement el, MonadIO m, MonadIO (Performable m), Reflex t
                   , MonadHold t m, TriggerEvent t m, PerformEvent t m
                   )
                => el
                -> Maybe  (AnimationTime -> IO ())
                   -- ^ rendering event is very special
                   --   We want to minimize the rendering time,
                   --   so this function does not add another indirection level
                   --   by wrapping the callback into newTriggerEvent
                -> m (AnimationHandler t)
registerHandler e mrenderCallback = do
    (pointerEventUp, pUpCallback) <- newTriggerEvent
    (pointerEventClick, pClickCallback) <- newTriggerEvent
    (pointerEventDown, pDownCallback) <- newTriggerEvent
    (pointerEventMove, pMoveCallback) <- newTriggerEvent
    (pointerEventCancel, pCancelCallback) <- newTriggerEvent
    (resizeEvent, resizeCallback) <- newTriggerEvent
    (wheelEvent, wheelCallback) <- newTriggerEvent
    (onRenderEvent, onRenderCallback) <- newTriggerEvent
    let renderCallback = case mrenderCallback of
            Nothing -> const $ pure ()
            Just rc -> rc
    renderFired <- liftIO $ newMVar ()
    performEvent_ $ (const . void . liftIO $ tryPutMVar renderFired ()) <$> onRenderEvent
    _state <- liftIO $ PK.newPointerKeeper (DOM.toElement e)
                           (\t -> do
                              renderCallback t
                              mr <- tryTakeMVar renderFired
                              case mr of
                                Nothing -> return ()
                                Just () -> onRenderCallback t
                           )
                            pUpCallback pClickCallback pDownCallback pMoveCallback pCancelCallback
                            resizeCallback
    liftIO $ PK.listenToWheel _state wheelCallback


    let -- EventSelector
        evSel :: forall a . AEventType a -> Event t a
        evSel (APointerEvent PUpEvent)     = pointerEventUp
        evSel (APointerEvent PDownEvent)   = pointerEventDown
        evSel (APointerEvent PMoveEvent)   = pointerEventMove
        evSel (APointerEvent PCancelEvent) = pointerEventCancel
        evSel (APointerEvent PClickEvent)  = pointerEventClick
        evSel AWheelEvent                  = wheelEvent
        evSel AResizeEvent                 = resizeEvent
        animationEvents = EventSelector evSel

        pEvSel :: forall a . PEventType a -> Event t a
        pEvSel PUpEvent     = pointerEventUp
        pEvSel PDownEvent   = pointerEventDown
        pEvSel PMoveEvent   = pointerEventMove
        pEvSel PCancelEvent = pointerEventCancel
        pEvSel PClickEvent  = pointerEventClick
        pointerEvents = EventSelector pEvSel

        shiftKeyB     = onDemand (PK.shiftKey _state)
        ctrlKeyB      = onDemand (PK.ctrlKey _state)
        altKeyB       = onDemand (PK.altKey _state)
        metaKeyB      = onDemand (PK.metaKey _state)
        buttonsB      = onDemand (PK.buttons _state)
        downPointersB = onDemand (PK.downPointers _state)
        curPointersB  = onDemand (PK.curPointers _state)
        downTimeB     = onDemand (PK.downTime _state)
        modKeysB      = onDemand (PK.modKeys _state)

    -- Dynamic for the size of a view port
    viewPortSizeI <- liftIO $ PK.viewPortSize _state
    viewPortSizeD <- Reflex.holdDyn viewPortSizeI (coerce <$> resizeEvent)


    return AnimationHandler {..}



data AnimationHandler t = AnimationHandler
  { _state :: PK.PointerKeeper
  , animationEvents :: EventSelector t AEventType
    -- ^ All animation events (pointer, wheel, and view port resize)
  , pointerEvents :: EventSelector t PEventType
    -- ^ All pointer events
  , onRenderEvent :: Event t AnimationTime
    -- ^ this event is fired every time after the main render callback function.
  , shiftKeyB     :: Behavior t Bool
    -- ^ Whether shift key is pressed
  , ctrlKeyB      :: Behavior t Bool
    -- ^ Whether ctrl Behavior is pressed
  , altKeyB       :: Behavior t Bool
    -- ^ Whether alt key is pressed
  , metaKeyB      :: Behavior t Bool
    -- ^ Whether meta key is pressed
  , buttonsB      :: Behavior t Int
    -- ^ Code of currecntly pressed button
  , downPointersB :: Behavior t [(Double,Double)]
    -- ^ List of pointers was pressed on last PointerDown event
  , curPointersB  :: Behavior t [(Double,Double)]
    -- ^ List of pointers currently pressed
  , downTimeB     :: Behavior t AnimationTime
    -- ^ Time of the last PointerDown event
  , modKeysB      :: Behavior t [ModKey]
    -- ^ All current modifier keys
  , viewPortSizeD :: Dynamic t (Double, Double)
    -- ^ State of the size of a target element
  , viewPortSizeI :: (Double,Double)
    -- ^ Initial size of a target element
  }

play :: MonadIO m => AnimationHandler t -> m ()
play = liftIO . PK.play . _state

pause :: MonadIO m => AnimationHandler t -> m ()
pause = liftIO . PK.pause . _state


-- | Mouse wheel up or down (arg is +1 or -1 for scroll up or down accordingly)
wheelEvents :: AnimationHandler t -> Event t WheelEvent
wheelEvents = flip select AWheelEvent . animationEvents

-- | when element is resized
resizeEvents :: AnimationHandler t -> Event t ResizeEvent
resizeEvents = flip select AResizeEvent . animationEvents



onDemand :: IO a -> Behavior t a
onDemand ma = SpiderBehavior . Spider.Behavior . Spider.BehaviorM . ReaderT $ computeF
  where
    {-# NOINLINE computeF #-}
    computeF (Nothing, _) = unsafeInterleaveIO ma
    computeF (Just (invW,_), _) = unsafeInterleaveIO $ do
        toReconnect <- newIORef []
        _ <- Spider.invalidate toReconnect [invW]
        -- from the function Reflex.Spider.Internal.invalidate it follows that
        -- the result invalidator list is empty,
        -- However, I am not sure what happens with toReconnect reference;
        -- invW should be InvalidatorPull, but its parents may be subscribed switches,
        -- which must be treated in a special way.
        -- On the other hand, I am not sure if any switch can be subscribed to this kind of behavior.
        ma

