{-# LANGUAGE ForeignFunctionInterface #-}

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

import Data.List (intercalate)
import Data.Maybe (listToMaybe, maybeToList)

import Melchior.DOM.Element (Element, Input)
import Melchior.DOM.Internal (DOM)
import Melchior.DOM.Node (Node)
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

foreign import js "runSelector(%1)"
    runSelector :: Selector [Element] b -> IO b


------------------------------------------------------------------------------
-- | Filters the input down to at most one node with the given id attribute.
byId :: (Node a, Nodes f) => String -> Selector (f a) (Maybe a)
byId eid = Selector $
    liftM toMaybe . filterIO (idEq (stringToJSString eid) . domNode)

foreign import js "idEq(%2, %1)"
    idEq :: JSString -> JSPtr Node -> IO Bool


------------------------------------------------------------------------------
-- | Filters the input to those nodes with the given class attribute.
byClass :: (Node a, Nodes f) => String -> Selector (f a) (f a)
byClass ecl = Selector $ filterIO (clEq (stringToJSString ecl) . domNode)

foreign import js "clEq(%2, %1)"
    clEq :: JSString -> JSPtr Node -> IO Bool


------------------------------------------------------------------------------
-- | Maps the input nodes to their children and concatenates the result.
children :: (Nodes f) => Selector (f Element) [Element]
children = Selector $ liftM (fmap Element) . concatMapIO (chlQ . domNode)

foreign import js "%1.childNodes"
    chlQ :: JSPtr Node -> IO [JSPtr Node]


------------------------------------------------------------------------------
-- | Filters the input elements to those capable of receiving input and
-- narrows their type.
inputs :: Nodes f => Selector (f Element) (f Input)
inputs = Selector $ liftM (fmap $ Input . unEl) . filterIO (inpF . domNode)

foreign import js "tag(%1, 'input')"
    inpF :: JSPtr Node -> IO Bool

