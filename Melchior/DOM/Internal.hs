------------------------------------------------------------------------------
-- | Defines the 'DOM' monad.
module Melchior.DOM.Internal
    ( -- * Monad
      DOM (DOM)

      -- * Nodes
    , Node (domNode)
    , JSNode
    ) where

------------------------------------------------------------------------------
import Control.Applicative (Applicative (..))
import Control.Monad.Fix (MonadFix (..))

import Melchior.JScript


------------------------------------------------------------------------------
-- | The monad for performing DOM operations in.  Acts as a state wrapper
-- over the document object.
newtype DOM a = DOM { unDOM :: IO a }

instance Functor DOM where
    fmap f = DOM . fmap f . unDOM

instance Applicative DOM where
    pure = DOM . pure
    (DOM f) <*> (DOM a) = DOM $ f <*> a

instance Monad DOM where
    return = DOM . return
    (DOM a) >>= k = DOM $ a >>= unDOM . k

instance MonadFix DOM where
    mfix f = DOM $ mfix (unDOM . f)


------------------------------------------------------------------------------
-- | Class for all node types.
class Node a where
    domNode :: a -> JSPtr JSNode


------------------------------------------------------------------------------
-- | Internal node definition.  Wraps over the JavaScript object.
data JSNode


