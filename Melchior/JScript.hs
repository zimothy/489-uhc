{-# LANGUAGE CPP, EmptyDataDecls #-}

------------------------------------------------------------------------------
-- | Reexports the UHC JavaScript types.  For non-UHC compilers, it exports
-- placeholder types to allow for type-checking compilation.
module Melchior.JScript
    ( JSPtr
    , JSString

    , jsStringToString
    , stringToJSString
    ) where

#ifdef __UTRECHT_HASKELL__
------------------------------------------------------------------------------
import Language.UHC.JScript.ECMA.String
    (JSString, jsStringToString, stringToJSString)
import Language.UHC.JScript.Primitives (JSPtr)

#else
------------------------------------------------------------------------------
data JSPtr a
data JSString

jsStringToString :: JSString -> String
jsStringToString = undefined

stringToJSString :: String -> JSString
stringToJSString = undefined

#endif

