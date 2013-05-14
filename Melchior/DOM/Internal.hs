------------------------------------------------------------------------------
-- | Defines the 'DOM' monad.
module Melchior.DOM.Internal
    (
      -- * Monad
      DOM
    ) where

------------------------------------------------------------------------------
import Control.Applicative (Applicative (..))
import Control.Monad.Fix (MonadFix (..))


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

