{-# LANGUAGE OverloadedStrings #-}

module LispVal (
    LispVal(..),
    Eval(..),
    IFunc(..),
    EnvCtx(..),
    LispException(..),
    showVal,
) where

import Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO)

data LispVal
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool deriving (Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving ( Monad
    , Functor
    , Applicative
    , MonadReader EnvCtx
    , MonadIO)

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
    showVal val =
        case val of
            (Atom atom)     -> atom
            (String str)    -> T.concat ["\"", str, "\""]
            (Number num)    -> T.pack $ show num
            (Bool True)     -> "#t"
            (Bool False)    -> "#f"
            Nil             -> "Nil"
            (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
            (Fun _)         -> "(internal function)"
            (Lambda _ _)    -> "(lambda function)"
        
data LispException
    = NumArgs Integer [LispVal]
    | LengthOfList T.Text Int
    | ExpectedList T.Text
    | TypeMismatch T.Text LispVal
    | BadSpecialForm T.Text
    | NotFunction LispVal
    | UnboundVar T.Text
    | Default LispVal
    | PError String -- from show anyway
    | IOError T.Text
    deriving (Typeable)

instance Show LispException where
    show = T.unpack . showError

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $  showVal <$> list

showError :: LispException -> T.Text
showError err =
    case err of
        (IOError txt)          -> T.concat ["Error reading file: ", txt]
        (NumArgs int args)     -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
        (LengthOfList txt int) -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
        (ExpectedList txt)     -> T.concat ["Error Expected List in funciton ", txt]
        (TypeMismatch txt val) -> T.concat ["Error Type Mismatch: ", txt, showVal val]
        (BadSpecialForm txt)   -> T.concat ["Error Bad Special Form: ", txt]
        (NotFunction val)      -> T.concat ["Error Not a Function: ", showVal val]
        (UnboundVar txt)       -> T.concat ["Error Unbound Variable: ", txt]
        (PError str)           -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
        (Default val)          -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]