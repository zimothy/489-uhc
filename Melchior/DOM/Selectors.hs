{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- | Defines functions for traversing the DOM to select certain elements.
module Melchior.DOM.Selectors
    ( -- * Type
      Selector

      -- * Running selectors
    , select

      -- * Building selectors
    , byId
    , byClass
    , children
    , inputs
    ) where

------------------------------------------------------------------------------
import Control.Category
import Control.Monad ((>=>), liftM)

import Data.Maybe (listToMaybe)

import Melchior.DOM.Element (Element (Element), Input (Input))
import Melchior.DOM.Internal (DOM (DOM), JSNode, Node (domNode))
import Melchior.JScript

import Prelude hiding ((.), id)


------------------------------------------------------------------------------
-- | Describes a type-safe selection of DOM nodes.
data Selector a b = Selector (a -> IO b)

instance Functor (Selector a) where
    fmap f (Selector g) = Selector $ liftM f . g

instance Category Selector where
    id = Selector return
    (Selector f) . (Selector g) = Selector $ g >=> f


------------------------------------------------------------------------------
-- | A collection of nodes.  Defined for '[]' and 'Maybe'.
class Functor f => Nodes f where
    concatMapIO :: (a -> IO [b]) -> f a -> IO [b]
    filterIO :: (a -> IO Bool) -> f a -> IO (f a)
    toMaybe :: f a -> Maybe a

instance Nodes Maybe where
    concatMapIO _ Nothing  = return []
    concatMapIO f (Just a) = f a
    filterIO _ Nothing  = return Nothing
    filterIO f (Just x) = do
        keep <- f x
        return $ if keep then Just x else Nothing
    toMaybe = id

instance Nodes [] where
    concatMapIO _ []       = return []
    concatMapIO f (x : xs) = do
        list <- f x
        liftM (list ++) $ concatMapIO f xs
    filterIO _ []       = return []
    filterIO f (x : xs) = do
        keep <- f x
        liftM (if keep then (x :) else id) $ filterIO f xs
    toMaybe = listToMaybe


------------------------------------------------------------------------------
-- | Runs a 'Selector' in the 'DOM' monad.
select :: Selector [Element] b -> DOM b
select = DOM . runSelector

#ifdef __UHC_TARGET_JS__
foreign import js "runSelector(%1)"
    runSelector :: Selector [Element] b -> IO b
#else
runSelector :: Selector [Element] b -> IO b
runSelector = undefined
#endif


------------------------------------------------------------------------------
-- | Filters the input down to at most one node with the given id attribute.
byId :: (Node a, Nodes f) => String -> Selector (f a) (Maybe a)
byId eid = Selector $
    liftM toMaybe . filterIO (idEq (stringToJSString eid) . domNode)

#ifdef __UHC_TARGET_JS__
foreign import js "idEq(%2, %1)"
    idEq :: JSString -> JSPtr JSNode -> IO Bool
#else
idEq :: JSString -> JSPtr JSNode -> IO Bool
idEq = undefined
#endif

------------------------------------------------------------------------------
-- | Filters the input to those nodes with the given class attribute.
byClass :: (Node a, Nodes f) => String -> Selector (f a) (f a)
byClass ecl = Selector $ filterIO (clEq (stringToJSString ecl) . domNode)

#ifdef __UHC_TARGET_JS__
foreign import js "clEq(%2, %1)"
    clEq :: JSString -> JSPtr JSNode -> IO Bool
#else
clEq :: JSString -> JSPtr JSNode -> IO Bool
clEq = undefined
#endif


------------------------------------------------------------------------------
-- | Maps the input nodes to their children and concatenates the result.
children :: Nodes f => Selector (f Element) [Element]
children = Selector $ liftM (fmap Element) . concatMapIO (chlQ . domNode)

#ifdef __UHC_TARGET_JS__
foreign import js "%1.childNodes"
    chlQ :: JSPtr JSNode -> IO [JSPtr JSNode]
#else
chlQ :: JSPtr JSNode -> IO [JSPtr JSNode]
chlQ = undefined
#endif


------------------------------------------------------------------------------
-- | Filters the input elements to those capable of receiving input and
-- narrows their type.
inputs :: Nodes f => Selector (f Element) (f Input)
inputs = Selector $ liftM (fmap $ Input . domNode) . filterIO (inpF . domNode)

#ifdef __UHC_TARGET_JS__
foreign import js "tag(%1, 'input')"
    inpF :: JSPtr JSNode -> IO Bool
#else
inpF :: JSPtr JSNode -> IO Bool
inpF = undefined
#endif

