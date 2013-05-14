------------------------------------------------------------------------------
-- | Defines the 'Element' type for general DOM elements as well as more
-- specific types that elements can be narrowed into.
module Melchior.DOM.Element
    ( -- * Element types
      Element
    ) where

------------------------------------------------------------------------------
import Melchior.DOM.Node
import Melchior.JScript


------------------------------------------------------------------------------
newtype Element = Element (JSPtr JSNode)

instance Node Element where
    domNode (Element a) = a


------------------------------------------------------------------------------
newtype Input = Input (JSPtr JSNode)

instance Node Input where
    domNode (Input a) = a


------------------------------------------------------------------------------
newtype Link = Link (JSPtr JSNode)

instance Node Link where
    domNode (Link a) = a

