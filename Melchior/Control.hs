module Melchior.Control
  ( -- * Types
    Signal
  , SF
    -- * Functions
  , createEventedSignal
  ) where

import Language.UHC.JScript.Primitives
import Language.UHC.JScript.ECMA.String (stringToJSString, JSString)
import Melchior.Dom
import Melchior.Dom.Events

data Signal a = Signal (JSPtr a)
type SF a b = Signal a -> Signal b

createEventedSignal :: (DomNode a) => a -> Event b -> Signal String
createEventedSignal el evt = primCreateEventedSignal el $ (stringToJSString . show) evt

foreign import js "Signals.createEventedSignal(%2, %3)"
  primCreateEventedSignal :: (DomNode a) => a -> JSString -> Signal String
