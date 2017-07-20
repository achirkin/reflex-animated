{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reflex.Dom.Widget.Animation.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Reflex.Dom.Widget.Animation.Types
  ( AnimationTime (..), getAnimationTime
    -- * Events
  , AEventType (..)
  , PEventType (..), asPointerEventType
  , IsPointerEvent (..), eventType
  , PointerEventType (..), PointerUp, PointerDown, PointerMove, PointerClick, PointerCancel
  , PointerEvent (..), pointers, eventJSType, button
  , ResizeEvent (..)
  , WheelEvent (..)
    -- * Modifiers
  , ModKey (..)
  ) where


import GHC.TypeLits (TypeError, ErrorMessage (..))

import GHCJS.Types (JSVal, JSString)
import GHCJS.Marshal.Pure
import JavaScript.Array (JSArray, toList)


-- | Current time in milliseconds (`performance.now()`)
foreign import javascript unsafe "performance.now()"
    getAnimationTime :: IO AnimationTime



-- | Animation event type mark
data AEventType a where
    APointerEvent :: PEventType (PointerEvent p) -> AEventType (PointerEvent p)
    AWheelEvent   :: AEventType WheelEvent
    AResizeEvent  :: AEventType ResizeEvent

data PEventType a where
    PUpEvent     :: PEventType (PointerEvent 'PointerUp)
    PDownEvent   :: PEventType (PointerEvent 'PointerDown)
    PMoveEvent   :: PEventType (PointerEvent 'PointerMove)
    PCancelEvent :: PEventType (PointerEvent 'PointerCancel)
    PClickEvent  :: PEventType (PointerEvent 'PointerClick)


-- | Unified representation for MouseEvent and TouchEvent in JS.
--   Uses JavaScript InteractiveElement.PointerEvent value
newtype PointerEvent (t :: PointerEventType) = PointerEvent JSVal

class IsPointerEvent (t :: PointerEventType) where
    -- | Get GADT tag of the pointer event type
    pEventType :: PointerEvent t -> PEventType (PointerEvent t)

-- | Convert GADT type to ordinary enum pointer event type
asPointerEventType :: PEventType (PointerEvent t) -> PointerEventType
asPointerEventType PUpEvent     = PointerUp
asPointerEventType PDownEvent   = PointerDown
asPointerEventType PMoveEvent   = PointerMove
asPointerEventType PCancelEvent = PointerCancel
asPointerEventType PClickEvent  = PointerClick

-- | Get term-level information about pointer event type
eventType :: IsPointerEvent t => PointerEvent t -> PointerEventType
eventType = asPointerEventType . pEventType
{-# INLINE eventType #-}

instance {-# OVERLAPPING #-} IsPointerEvent PointerUp     where pEventType _ = PUpEvent
instance {-# OVERLAPPING #-} IsPointerEvent PointerDown   where pEventType _ = PDownEvent
instance {-# OVERLAPPING #-} IsPointerEvent PointerMove   where pEventType _ = PMoveEvent
instance {-# OVERLAPPING #-} IsPointerEvent PointerCancel where pEventType _ = PCancelEvent
instance {-# OVERLAPPING #-} IsPointerEvent PointerClick  where pEventType _ = PClickEvent
instance {-# OVERLAPPABLE #-}
    TypeError ('Text "Add IsPointerEvent constraint or specify event type explicitly to call eventType function.")
    => IsPointerEvent t where
    pEventType _ = error "Add IsPointerEvent constraint or specify event type explicitly to call eventType function."


-- | Type of a pointer event
data PointerEventType
  = PointerUp
  | PointerDown
  | PointerMove
  | PointerCancel
  | PointerClick
  deriving (Eq, Enum, Show, Read)

type PointerUp     = 'PointerUp
type PointerDown   = 'PointerDown
type PointerMove   = 'PointerMove
type PointerCancel = 'PointerCancel
type PointerClick  = 'PointerClick

-- | Mouse wheel event
data WheelEvent = WheelUp | WheelDown
  deriving (Eq,Show,Ord)
instance PFromJSVal WheelEvent where
    pFromJSVal j = if (pFromJSVal j :: Double) >= 0 then WheelUp else WheelDown
instance PToJSVal WheelEvent where
    pToJSVal WheelUp = pToJSVal (1 :: Double)
    pToJSVal WheelDown = pToJSVal (-1 :: Double)


newtype ResizeEvent = ResizeEvent (Double,Double)

-- | Time of events is Double.
--   This is the time returned by JS function `performance.now()`
--   or passed as an argument to `requestAnimationFrame` callback.
newtype AnimationTime = AnimationTime Double
  deriving (Eq,Ord,Show,Num,Real,RealFrac,RealFloat,Fractional,Floating)
instance PFromJSVal AnimationTime where pFromJSVal = AnimationTime . pFromJSVal
instance PToJSVal AnimationTime where pToJSVal (AnimationTime t) = pToJSVal t


-- | Positions of all pointers
pointers :: PointerEvent t -> [(Double,Double)]
pointers = fmap js_unpackCoords2D . toList . js_pointers
foreign import javascript unsafe "$1.pointers" js_pointers  :: PointerEvent t -> JSArray -- JS.Array Coords2D
-- | JS type of event
foreign import javascript unsafe "$1.type"     eventJSType :: PointerEvent t -> JSString
-- | Id of a mouse button pressed (or zero for touches)
foreign import javascript unsafe "$1.button"   button    :: PointerEvent t -> Int




-- | A modifier key. A list of modifier keys is reported for every key press
-- and mouse click.
data ModKey = Shift | Ctrl | Alt | Meta
  deriving (Show, Read, Eq, Bounded, Enum)


-- | Get pointer coordinates
foreign import javascript unsafe "$r1=$1[0];$r2=$1[1];" js_unpackCoords2D :: JSVal -> (Double, Double)
