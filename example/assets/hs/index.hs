import Control.Category hiding ((>>>), (<<<))
import Melchior.Control
import Melchior.Data.List
import Melchior.Dom
import Melchior.Dom.Events
import Melchior.Dom.Selectors

import Language.UHC.JScript.ECMA.String (JSString, stringToJSString, jsStringToString)
import Prelude hiding ((.), id, head, map)

main :: IO Element
main = runDom $ \html -> do
  setupNavLinks html
  setupClickListeners html
  setupMainButton html
  return $ toElement html

setupNavLinks :: Document -> Dom Element
setupNavLinks = \html -> do
  --element selection is here
  content <- head $ get (Selector $ byId "jumbotron") $ [toElement html]
  links <- get (Selector $ byClass "link") $ [toElement html]
  --signal creation is here
  clickEvents <- return $ map clickListener links
  --and we can then pipe signals of events through a function
  return $ map (removeSiblingClass "active" >>>
                addClassToParent "active" >>>
                addClassToSiblings "hidden" >>>
                removeClassFrom "hidden" >>>
                terminal) $ map liftSignal clickEvents
  head $ return links

terminal :: SF (Dom a) (Dom a)
terminal s = pipe s (\x -> x)

setupClickListeners :: Document -> Dom Element
setupClickListeners = \html -> do
  checkBoxes <- get (Selector $ byClass "check") $ [toElement html]
  return $ map strike $ map clickListener checkBoxes
  head $ return checkBoxes

setupMainButton :: Document -> Dom Element
setupMainButton = \html -> do
  mainButton <- head $ get (Selector $ byClass "btn-success") $ [toElement html]
  return $ strike $ clickListener mainButton
  return mainButton

clickListener :: Element -> Signal JSString
clickListener e = createEventedSignalOf (Of $ stringToJSString "jsstring") e (MouseEvt ClickEvt) "innerHTML"

addClassToParent :: String -> SF (Dom JSString) (Dom JSString)
addClassToParent cls s = pipe s (\x -> passThrough x $! signalIO $ do
                                       y <- x
                                       elem <- head $ get (Selector $ byClass $ jsStringToString y) [toElement document]
                                       return $! addClass (stringToJSString cls) $ parentOf $ elem
                                       x)

removeSiblingClass :: String -> SF (Dom JSString) (Dom JSString)
removeSiblingClass cls s = pipe s (\x -> passThrough x $! signalIO $ do
                                         y <- x
                                         elem <- head $ get (Selector $ byClass $ jsStringToString y) [toElement document]
                                         return $! map stripClass $ siblings $ parentOf $ elem
                                         x)
                          where
                            stripClass = (removeClass $ stringToJSString cls)

liftSignal :: Signal a -> Signal (Dom a)
liftSignal s = pipe s (\x -> pass (stringToJSString "lifted") $ return $! x)

addClassToSiblings :: String -> SF (Dom JSString) (Dom JSString)
addClassToSiblings cls s = pipe s (\z -> passThrough z $! signalIO $ do
                                      y <- z
                                      elem <- head $ get (Selector $ byId $ jsStringToString y) [toElement document]
                                      return $! map (addClass $ stringToJSString cls) $ siblings elem
                                      z)

                                        
removeClassFrom :: String -> SF (Dom JSString) (Dom JSString)
removeClassFrom  cls s = pipe s (\x -> signalIO $ do
                                    y <- x
                                    elem <- head $ get (Selector $ byId $ jsStringToString y) [toElement document]
                                    return $! removeClass (stringToJSString cls) elem
                                    return $! y)
                
strike :: SF JSString JSString
strike s = pipe s (\x -> toggle (stringToJSString "checked") $ UHC.Base.head $ siblings $ source s)

--
--here be dragons / unimplementedness

foreign import js "log(%2, %1)"
  log :: Element -> JSString -> Dom ()

foreign import js "log(%2, %1)"
  pass :: JSString -> a -> a
