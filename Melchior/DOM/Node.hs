------------------------------------------------------------------------------
-- | Contains definitions for dealing with the internal definitions of HTML
-- nodes.  All representations of nodes in the DOM tree should be instances
-- of the 'Node' type class.
module Melchior.DOM.Node
    ( -- * Class
      Node (domNode)

      -- * Internal node type
    , JSNode
    )
    where

------------------------------------------------------------------------------
import Melchior.JScript


------------------------------------------------------------------------------
-- | Class for all node types.
class Node a where
    domNode :: a -> JSPtr JSNode


------------------------------------------------------------------------------
-- | Internal node definition.  Wraps over the JavaScript object.
data JSNode

